module XML

open System
open System.IO
open System.Reflection
open System.Text
open System.Xml
open System.Xml.Linq
open FSharp.Data
open FSharp.Data.Runtime
open System.Xml.Schema

[<Literal>]
let private AbsoluteResolutionFolder = __SOURCE_DIRECTORY__ + "\\xmlschm1-24-1"

[<Literal>]
let LayoutTopologieSchema = "layout-topologie.xsd"

[<Literal>]
let private AbsoluteLayoutTopologieSchema =
    AbsoluteResolutionFolder + "\\" + LayoutTopologieSchema

type LayoutTopologie =
    XmlProvider<Schema=AbsoluteLayoutTopologieSchema, ResolutionFolder=AbsoluteResolutionFolder, Global=true>

let toCurrency (x: double) = x.ToString("0.00")

let encoding = Encoding.ASCII

let validateXElement (xElement: XElement) (schemaFile: string) =
    let schema =
        let resolutionFolder =
            let cb = Assembly.GetExecutingAssembly().Location

            let path = Uri.UnescapeDataString((UriBuilder(cb)).Path)

            Path.GetDirectoryName(path)

        File.OpenText(schemaFile)
        |> XmlSchema.parseSchemaFromTextReader resolutionFolder

    XDocument(xElement).Document.Validate(schema, null)

let private saveXElement (xElement: XElement) (xmlFileName: string) =
    let xmlWriterSettings = XmlWriterSettings(Encoding = encoding, Indent = true)

    using (XmlTextWriter.Create(xmlFileName, xmlWriterSettings)) xElement.Save

let validateAndSaveXElement (xElement: XElement) (xmlFileName: string) =
    saveXElement xElement xmlFileName
    validateXElement xElement LayoutTopologieSchema
