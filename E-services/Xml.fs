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
let private RelativeResolutionFolder = "xmlschm1-25-5"

[<Literal>]
#if WINDOWS
let private AbsoluteResolutionFolder =
    __SOURCE_DIRECTORY__ + "\\" + RelativeResolutionFolder
#else
let private AbsoluteResolutionFolder =
    __SOURCE_DIRECTORY__ + "/" + RelativeResolutionFolder
#endif

[<Literal>]
let LayoutTopologieSchema = "T619_T4.xsd"

[<Literal>]
let private AbsoluteLayoutTopologieSchema =
#if WINDOWS
    AbsoluteResolutionFolder + "\\" + LayoutTopologieSchema
#else
    AbsoluteResolutionFolder + "/" + LayoutTopologieSchema
#endif

type LayoutTopologie =
    XmlProvider<Schema=AbsoluteLayoutTopologieSchema, ResolutionFolder=AbsoluteResolutionFolder, Global=true>

let toCurrency (x: double) = x.ToString("0.00")

let encoding = Encoding.ASCII

let validateXElement (xElement: XElement) (schemaFile: string) =
    let schema =
        let resolutionFolder =
            let cb = Assembly.GetExecutingAssembly().Location

            let path =
                if OperatingSystem.IsWindows() then
                    Uri.UnescapeDataString((UriBuilder(cb)).Path)
                else
                    cb

            Path.Combine(Path.GetDirectoryName(path), RelativeResolutionFolder)

        File.OpenText(Path.Combine(resolutionFolder, schemaFile))
        |> XmlSchema.parseSchemaFromTextReader resolutionFolder

    XDocument(xElement).Document.Validate(schema, null)

let private saveXElement (xElement: XElement) (xmlFileName: string) =
    let xmlWriterSettings = XmlWriterSettings(Encoding = encoding, Indent = true)

    using (XmlTextWriter.Create(xmlFileName, xmlWriterSettings)) xElement.Save

let validateAndSaveXElement (xElement: XElement) (xmlFileName: string) =
    saveXElement xElement xmlFileName
    validateXElement xElement LayoutTopologieSchema
