open XML


type FullName =
    { surName: string
      givenName: string
      middleInitial: string }

    override x.ToString() =
        x.givenName + " " + x.middleInitial + " " + x.surName

type Country = CAN
type Province = BC

type Address =
    { street: string
      city: string
      province: Province
      country: Country
      postalCode: string }

type Income =
    { employmentIncome: double
      cppeContribution: option<double>
      cppContribution: double
      employeeEmploymentInsurance: option<double>
      employmentInsuranceEarn: double
      cppQppEarn: double
      federalIncomeTax: double
      provincialIncomeTax: double }

type SocialInsuranceNumber = string

let fullNameToEmpeNm (n: FullName) =
    LayoutTopologie.EmpeNm(snm = n.surName, gvnNm = Some n.givenName, init = Some n.middleInitial)

let fullNameToTransmitterName (transmitterName: FullName) =
    LayoutTopologie.TransmitterName(l1Nm = transmitterName.ToString())

let addressToEmpeAddr (a: Address) =
    LayoutTopologie.EmpeAddr(
        addrL1Txt = Some a.street,
        addrL2Txt = None,
        ctyNm = Some a.city,
        provCd = Some(string a.province),
        cntryCd = Some(string a.country),
        pstlCd = Some a.postalCode
    )

let incomeToT4Amt (i: Income) =
    LayoutTopologie.T4Amt(
        emptIncamt = (i.employmentIncome |> toCurrency |> Some),
        cppCntrbAmt = (i.cppContribution |> toCurrency |> Some),
        cppeCntrbAmt = (i.cppeContribution |> Option.map toCurrency),
        qppCntrbAmt = None,
        qppeCntrbAmt = None,
        empeEipAmt = (i.employeeEmploymentInsurance |> Option.map toCurrency),
        rppCntrbAmt = None,
        itxDdctAmt = ((i.federalIncomeTax + i.provincialIncomeTax) |> toCurrency |> Some),
        eiInsuErnAmt = (i.employmentInsuranceEarn |> toCurrency |> Some),
        cppQppErnAmt = (i.cppQppEarn |> toCurrency |> Some),
        unnDuesAmt = None,
        chrtyDonsAmt = None,
        padjAmt = None,
        provPipAmt = None,
        provInsuErnAmt = None
    )

type BusinessNumber = string

type ReportType =
    | Originals
    | Amendments
    | Cancel

let reportTypeToCode =
    function
    | Originals -> "O"
    | Amendments -> "A"
    | Cancel -> "C"

type T4Info =
    { employeeSIN: SocialInsuranceNumber
      employeeNumber: int
      businessNumber: BusinessNumber
      isCppOrQppExempt: bool
      isEIExempt: bool
      provinceOfEmployment: Province
      reportType: ReportType }

let t4InfoToT4Slip
    (t4Info: T4Info)
    (empeNm: LayoutTopologie.EmpeNm)
    (empeAddr: LayoutTopologie.EmpeAddr)
    (t4Amt: LayoutTopologie.T4Amt)
    =
    LayoutTopologie.T4Slip(
        empeNm = empeNm,
        empeAddr = Some empeAddr,
        sin = t4Info.employeeSIN,
        empeNbr = Some(string t4Info.employeeNumber),
        bn = t4Info.businessNumber,
        rppDpspRgstNbr = None,
        cppQppXmptCd = (if t4Info.isCppOrQppExempt then "1" else "0"),
        eiXmptCd = (if t4Info.isEIExempt then "1" else "0"),
        provPipXmptCd = None,
        emptCd = None,
        rptTcd = (reportTypeToCode t4Info.reportType),
        emptProvCd = string t4Info.provinceOfEmployment,
        emprDntlBenRptCd = None,
        t4Amt = Some t4Amt,
        othInfo = None
    )

let employerNameToEmprNm (employerName: string) =
    LayoutTopologie.EmprNm(l1Nm = employerName, l2Nm = None, l3Nm = None)

let addressToEmprAddr (a: Address) =
    LayoutTopologie.EmprAddr(
        addrL1Txt = Some a.street,
        addrL2Txt = None,
        ctyNm = Some a.city,
        provCd = Some(string a.province),
        cntryCd = Some(string a.country),
        pstlCd = Some a.postalCode
    )

type Phone =
    { areaCode: string
      phoneNumber: string }

type Contact = { name: FullName; phone: Phone }

let contactToCntc2 (c: Contact) =
    LayoutTopologie.Cntc2(
        cntcNm = c.name.ToString(),
        cntcAreaCd = c.phone.areaCode,
        cntcPhnNbr = c.phone.phoneNumber,
        cntcExtnNbr = None
    )

let preparerSINToPprtrSin (preparerSIN: SocialInsuranceNumber) =
    LayoutTopologie.PprtrSin(pprtr1Sin = preparerSIN, pprtr2Sin = None)

let t4SlipsToT4Tamt (t4Slips: array<LayoutTopologie.T4Slip>) =
    let t4Amts = t4Slips |> Array.choose (fun x -> x.T4Amt)

    let sumT4Amts f =
        let amts = t4Amts |> Array.choose f

        match amts with
        | [||] -> None
        | _ -> amts |> Array.sumBy double |> toCurrency |> Some

    match t4Amts with
    | [||] -> None
    | _ ->
        LayoutTopologie.T4Tamt(
            totEmptIncamt = sumT4Amts (fun x -> x.EmptIncamt),
            totEmpeCppAmt = sumT4Amts (fun x -> x.CppCntrbAmt),
            totEmpeCppeAmt = sumT4Amts (fun x -> x.CppeCntrbAmt),
            totEmpeEipAmt = sumT4Amts (fun x -> x.EmpeEipAmt),
            totRppCntrbAmt = sumT4Amts (fun x -> x.RppCntrbAmt),
            totItxDdctAmt = sumT4Amts (fun x -> x.ItxDdctAmt),
            totPadjAmt = sumT4Amts (fun x -> x.PadjAmt),
            totEmprCppAmt = sumT4Amts (fun x -> x.CppCntrbAmt),
            totEmprCppeAmt = sumT4Amts (fun x -> x.CppeCntrbAmt),
            totEmprEipAmt = sumT4Amts (fun x -> x.EmpeEipAmt)
        )
        |> Some

type T4Summary =
    { businessNumber: BusinessNumber
      taxYear: int
      reportTypeCode: ReportType
      preparerSIN: SocialInsuranceNumber }

let t4SlipsSummaryToT4
    (t4Summary: T4Summary)
    (emprNm: LayoutTopologie.EmprNm)
    (emprAddr: LayoutTopologie.EmprAddr)
    (cntc2: LayoutTopologie.Cntc2)
    (t4Slips: array<LayoutTopologie.T4Slip>)
    =
    let t4Summary =
        LayoutTopologie.T4Summary(
            bn = t4Summary.businessNumber,
            emprNm = emprNm,
            emprAddr = Some emprAddr,
            cntc = cntc2,
            txYr = string t4Summary.taxYear,
            slpCnt = (t4Slips |> Array.length |> string),
            pprtrSin = Some(preparerSINToPprtrSin t4Summary.preparerSIN),
            rptTcd = reportTypeToCode t4Summary.reportTypeCode,
            fileramendmentnote = None,
            t4Tamt = t4SlipsToT4Tamt t4Slips
        )

    LayoutTopologie.T4(t4Slips = t4Slips, t4Summary = t4Summary)

type Email = string

let contactToCntc (contact: Contact) (email: Email) =
    LayoutTopologie.Cntc(
        cntcNm = contact.name.ToString(),
        cntcAreaCd = contact.phone.areaCode,
        cntcPhnNbr = contact.phone.phoneNumber,
        cntcExtnNbr = None,
        cntcEmailArea = email,
        secCntcEmailArea = None
    )

type Language =
    | English
    | French

    member this.ToShortCode() =
        match this with
        | English -> "E"
        | French -> "F"

type TransmitterAccountNumber =
    | BN9 of BusinessNumber
    | BN15 of BusinessNumber
    | NR4 of BusinessNumber
    | Trust of BusinessNumber

let transmitterAccountNumberToLayoutTopologie =
    function
    | BN9 x -> LayoutTopologie.TransmitterAccountNumber(bn9 = Some x, bn15 = None, nr4 = None, trust = None)
    | BN15 x -> LayoutTopologie.TransmitterAccountNumber(bn9 = None, bn15 = Some x, nr4 = None, trust = None)
    | NR4 x -> LayoutTopologie.TransmitterAccountNumber(bn9 = None, bn15 = None, nr4 = Some x, trust = None)
    | Trust x -> LayoutTopologie.TransmitterAccountNumber(bn9 = None, bn15 = None, nr4 = None, trust = Some x)


type TransmitInfo =
    { submissionReferenceIdentification: string
      reportType: ReportType
      transmitterAccountNumber: TransmitterAccountNumber
      country: Country
      language: Language }

let transmitInfoToT619
    (t: TransmitInfo)
    (transmitterName: LayoutTopologie.TransmitterName)
    (cntc: LayoutTopologie.Cntc)
    =
    LayoutTopologie.T619(
        transmitterAccountNumber = (t.transmitterAccountNumber |> transmitterAccountNumberToLayoutTopologie),
        transmitterRepId = None,
        sbmtRefId = Some t.submissionReferenceIdentification,
        summCnt = Some "1",
        langCd = (t.language.ToShortCode() |> Some),
        transmitterName = Some transmitterName,
        transmitterCountryCode = (t.country |> string),
        cntc = cntc
    )

let t4ToReturn (t4: LayoutTopologie.T4) = LayoutTopologie.Return(t4 = Some t4)

let submission =
    let businessNumber = "999999999RP0001"
    let country = Country.CAN
    let reportType = ReportType.Originals

    let employeeName =
        { surName = "Nixon"
          givenName = "Richard"
          middleInitial = "M" }

    let employeeAddress =
        { street = "9999 Fake Street"
          city = "Kelowna"
          province = Province.BC
          country = country
          postalCode = "A1A1A1" }

    let contact =
        { name = employeeName
          phone =
            { areaCode = "999"
              phoneNumber = "999-9999" } }

    let t619 =
        let transmitInfo =
            { submissionReferenceIdentification = "AaaAAA9a"
              reportType = reportType
              transmitterAccountNumber = TransmitterAccountNumber.BN15(businessNumber)
              country = country
              language = Language.English }

        let cntc =
            let contactEmail = "contact@consoto.com"
            contactToCntc contact contactEmail

        let transmitterName = employeeName |> fullNameToTransmitterName

        transmitInfoToT619 transmitInfo transmitterName cntc

    let t4 =

        let t4slip =
            let t4Info =
                { employeeSIN = "999999999"
                  employeeNumber = 1
                  businessNumber = businessNumber
                  isCppOrQppExempt = false
                  isEIExempt = true
                  provinceOfEmployment = Province.BC
                  reportType = reportType }

            let t4Amt =
                let income =
                    { employmentIncome = 400000.0
                      cppeContribution = None
                      cppContribution = 3754.45
                      employeeEmploymentInsurance = None
                      employmentInsuranceEarn = 0
                      cppQppEarn = 66600.0
                      federalIncomeTax = 104862.0
                      provincialIncomeTax = 59099.0 }

                incomeToT4Amt income

            let empeNm = fullNameToEmpeNm employeeName
            let empeAddr = addressToEmpeAddr employeeAddress
            t4InfoToT4Slip t4Info empeNm empeAddr t4Amt

        let t4Summary =
            { businessNumber = businessNumber
              taxYear = 2023
              reportTypeCode = ReportType.Originals
              preparerSIN = "999999999" }

        let cntc2 = contactToCntc2 contact

        let emprNm =
            let employerName = "Contoso, LTD"
            employerNameToEmprNm employerName

        let emprAddr =
            let employerAddress = employeeAddress
            addressToEmprAddr employerAddress

        t4SlipsSummaryToT4 t4Summary emprNm emprAddr cntc2 [| t4slip |]

    let ret = t4ToReturn t4
    LayoutTopologie.Submission(t619, returns = [| ret |])

validateAndSaveXElement submission.XElement $"{nameof submission}.xml"
