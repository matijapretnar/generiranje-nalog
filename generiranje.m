(* ::Package:: *)

(* ::Subsection:: *)
(*Pomo\[ZHacek]ni ukazi*)


ImeStudenta[student_] := student[[3]] <> " " <> student[[2]]
ImeDatoteke[student_] := 
  StringReplace[
    student[[2]] <> student[[3]],
     {" " -> "",
      "\[CHacek]" -> "c", "\[CapitalCHacek]" -> "C",
      "\[CAcute]" -> "c", "\[CapitalCAcute]" -> "C",
      "\:0111" -> "d", "\:0110" -> "D",
      "\[SHacek]" -> "s", "\[CapitalSHacek]" -> "S",
      "\[ZHacek]" -> "z", "\[CapitalZHacek]" -> "Z"}
  ]
VpisnaStevilka[student_] := student[[1]]


(* ::Subsection:: *)
(*Sestavljanje nalog*)


Opusti[] := Throw[Null]


preveri::nedrzi = "Pogoj `1` ne drzi.";
Preveri[pogoj_] :=
  If[Not[TrueQ[pogoj]], Message[preveri::nedrzi, HoldForm[pogoj]]; Opusti[]]
SetAttributes[Preveri, HoldFirst]


PripraviNalogo[problem_, student_] :=
  Module[{naloga = Null},
    SeedRandom[VpisnaStevilka[student]];
    Quiet[While[naloga == Null, naloga = Catch[problem[student]]]];
    naloga
  ]


PrimeriNaloge[problem_, stevilo_:7] :=
  TableForm[
    Table[
      PripraviNalogo[problem, {27001000 + i, "Novak " <> ToString[i], "Janez"}],
      {i, stevilo}
    ],
    TableDepth -> 2
  ]


PripraviNaloge[problemi_, studentje_] := 
  Module[{i = 0},
    SetSharedVariable[i];
    Monitor[
      ParallelTable[{student,
        ++i;
        Table[Quiet[PripraviNalogo[problem, student]], {problem, 
        First /@ problemi}]}, {student, studentje}], 
      ProgressIndicator[i, {1, Length[studentje]}]
    ]
  ]


(* ::Subsection:: *)
(*Izpisovanje nalog*)


IzpisiNalogo[naloge_, parametri_, pot_, datotekaNaloge_] :=
  Module[{datoteke},
    datoteke = ReadList[datotekaNaloge, Word, RecordLists -> True,
           RecordSeparators -> "%============================================================================%",
           WordSeparators -> "%----------------------------------------------------------------------------%"];
    Table[
      IzpisiDatoteko[naloge, parametri, FileNameJoin[{pot, First[datoteka]}], FileExtension[datotekaNaloge], Rest[datoteka]],
      {datoteka, StringTrim /@ datoteke}
    ]
  ]


IzpisiDatoteko[naloge_, parametri_, pot_, koncnica_, {template_}] :=
  Module[{datoteka},
    Quiet[CreateDirectory[pot]];
    DeleteFile[FileNames[FileNameJoin[{pot, "*"}]]];
    ParallelDo[
      Block[{student = naloga[[1]]},
        datoteka = OpenWrite[FileNameJoin[{pot, ImeDatoteke[student] <> "." <> koncnica}], BinaryFormat -> True];
        Izpisi[parametri, naloga[[2]], template, datoteka]
        Close[datoteka]
      ],
    {naloga, naloge}];
  Return["\[Dash] mapo " <> pot]
 ]



IzpisiDatoteko[naloge_, parametri_, pot_, koncnica_, {}] :=
  Sequence[]


IzpisiDatoteko[naloge_, parametri_, pot_, koncnica_, {header_, template_, footer_}] :=
  Module[{datoteka},
    datoteka = OpenWrite[pot, BinaryFormat -> True];
    BinaryWrite[datoteka, header];
    Do[
      Block[{student = naloga[[1]]},
        Izpisi[parametri, naloga[[2]], template <> "\n", datoteka]
      ],
    {naloga, naloge}];
    BinaryWrite[datoteka, footer];
    Close[datoteka];
    Return["\[Dash] datoteko " <> pot]
  ]


Izpisi[parametri_, vrednostiParametrov_, template_, channel_] :=
  With[{vsiParametri = Flatten[parametri]},
    Block[vsiParametri,
      parametri = vrednostiParametrov;
      Module[{tempId, tempFileIn, tempFileOut},
        tempId = ToString[RandomInteger[100000]];
        tempFileIn = FileNameJoin[{$TemporaryDirectory, "in" <> tempId}];
        tempFileOut = FileNameJoin[{$TemporaryDirectory, "out" <> tempId}];
        BinaryWrite[tempFileIn, template]; Close[tempFileIn];
        Splice[tempFileIn, tempFileOut, FormatType -> StandardForm, PageWidth -> Infinity];
        BinaryWrite[channel, BinaryReadList[tempFileOut]];
        DeleteFile[{tempFileIn, tempFileOut}]
      ]
    ]
  ]


SestaviNaloge[datotekaNaloge_, nalog___, OptionsPattern[{
  CiljniDirektorij -> NotebookDirectory[], SeznamStudentov -> "studentje.csv", KodnaTabela -> "UTF8"}]] :=
  Module[{studentje, podatki, encoding, directory, naloge = List[nalog], pot, izpisi, ustvarjene},
    encoding = $CharacterEncoding;
    directory = Directory[];
    $CharacterEncoding = OptionValue[KodnaTabela];
    SetDirectory[NotebookDirectory[]];
    studentje = Import[OptionValue[SeznamStudentov], CharacterEncoding -> $CharacterEncoding];
    podatki = PripraviNaloge[naloge, studentje];
    pot = FileNameJoin[{OptionValue[CiljniDirektorij], FileBaseName[datotekaNaloge]}];

    izpisi = DialogInput[Column[{
      Manipulate[
        Column[Table[
         Panel[
           TableForm[{naloge[[j, 2]], podatki[[i, 2, j]]},
           TableDepth -> 2, TableDirections -> Row, TableAlignments -> Top],
           naloge[[j, 1]]
         ], {j, Length[naloge]}]],
      {{i, 1, "\[SHacek]tudent"}, 1, Length[studentje], 1},
      Dynamic[ImeStudenta[studentje[[i]]]]
      ],
      TextCell["Ste zadovoljni z nalogami?"],
      Row[{
        Button["Ne, prekini sestavljanje", DialogReturn[False]],
        DefaultButton["Da, nadaljuj z izpisom datotek", DialogReturn[True]]
      }]
    }, Left]];
    While[izpisi,
      ustvarjene = IzpisiNalogo[podatki, Last /@ naloge, pot, datotekaNaloge];
      izpisi = DialogInput[Column[{
        TextCell["Ustvaril sem:"],
        Sequence @@ (TextCell /@ ustvarjene),
        TextCell["Ste zadovoljni z ustvarjenimi datotekami?"],
        Row[{
          Button["Ne, ponovno ustvari datoteke", DialogReturn[True]],
          DefaultButton["Da, kon\[CHacek]aj s sestavljanjem", DialogReturn[False]]
        }]
      }, Left]]
    ];
    $CharacterEncoding = encoding;
    SetDirectory[directory];
    Null
  ]
