
module Adrienne.Main


open System
open System.IO
open System.Collections.Generic
open System.Text
open YamlDotNet.RepresentationModel
open System.Xml

type YamlObject = 
    | Asset
    | NwsPost
    | EvtEvent
    | Outlander

type OutputDocs = Dictionary<string,XmlDocument>


let yamlObjectYpe (line:string):YamlObject = 
    let item = (line.Split( [|':'|], 2)).[1].Trim()
    match item with
        | "Asset" -> Asset
        | "NwsPost" -> NwsPost
        | "EvtEvent" -> EvtEvent 
        | _     -> Outlander  // a match antyhing match


let printYamlObject(y:YamlObject):string = 
    match y with 
        | Asset -> "Asset"
        | NwsPost -> "NwsPost"
        | EvtEvent -> "EvtEvent"
        | Outlander -> "Outlander"



let classObject (line:string):YamlObject option = 
    match (line.StartsWith "--- !ruby/object") with 
        | false -> None
        | true -> Some (yamlObjectYpe(line))

     
let documents (fn:YamlObject -> string -> OutputDocs -> unit) (stream:StreamReader) (outDocs:OutputDocs)  =
    let mutable (currentYamlObj:YamlObject option) = None
    let mutable document:string = ""
    
    while not stream.EndOfStream do
        let line = stream.ReadLine()

        match (classObject line) with 
            | Some nextYamlObject -> 
                match currentYamlObj with 
                    | Some y -> fn y document outDocs
                    | None -> ()
                currentYamlObj <- Some nextYamlObject
                document <- line + "\n"
            | None -> 
                document <- 
                    document + line + "\n"
   
    match currentYamlObj with
        | Some y -> fn y document outDocs
        | None -> ()

let mappingKey (entry:KeyValuePair<YamlNode,YamlNode>) : string =
    (entry.Key :?> YamlScalarNode).Value

let rec toXml (root:YamlNode) (xmlRoot:XmlNode) (xmlDocument:XmlDocument) =
    match root with
        | :? YamlMappingNode as mapping ->
                    for entry in mapping.Children do
                        let e = xmlDocument.CreateElement(mappingKey entry)
                        let r = xmlRoot.AppendChild(e)
                        toXml entry.Value r xmlDocument
                                    
        | :? YamlScalarNode as scalar ->
                    let v = scalar.Value
                    xmlRoot.InnerText <- v
        | :? YamlSequenceNode as seq ->
                    ignore (Seq.iter (fun n -> toXml n xmlRoot xmlDocument) (seq.Children))
        | _ -> printf "Unrecognized node: %s" (root.ToString())
    printfn ""

let createWriter (filename:string) : XmlWriter =
    let settings = new XmlWriterSettings()
    settings.Indent <- true
    XmlWriter.Create("../../" + filename + ".xml", settings)

let parseDocument (yamlObject:YamlObject) (document:string) (outDocs:OutputDocs) =
    match yamlObject with
        | Asset | EvtEvent | NwsPost -> 
            let xmlDocument = outDocs.[printYamlObject yamlObject]
            let root = xmlDocument.FirstChild.AppendChild(xmlDocument.CreateElement(printYamlObject yamlObject))
            let yaml = new YamlStream()
            let r = new StringReader(document)
            yaml.Load(r)
            toXml (yaml.Documents.[0].RootNode) root xmlDocument
        | _ -> ()

[<EntryPoint>]
let main args = 
    let yamlReader = new StreamReader(args.[0])
    let outDocs = new OutputDocs()

    let createDoc name = 
        let d = new XmlDocument()
        d.AppendChild(d.CreateElement(name + "s")) |> ignore
        outDocs.Add(name, d)

    let writeOut name =                
        printfn "Wrote %s.xml" name
        let out = outDocs.[name]
        use w = createWriter name
        out.WriteTo(w)
    
    let parseTo = documents parseDocument yamlReader 

    let createDocs names =
        names |> Seq.iter createDoc
        parseTo outDocs
        names |> Seq.iter writeOut

    (["Asset"; "NwsPost"; "EvtEvent";]) |> createDocs
    0



