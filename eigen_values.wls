#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 250 * {4, 3};
exportToPov = False;

n = 8;
repeat = 100000;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

betaParam = 0.001;
(* randomVector[n_] := Table[RandomReal[] * 2 - 1, {k, n}]; *)
randomVector[n_] := Table[RandomVariate[BetaDistribution[betaParam, betaParam]] * 2 - 1, {k, n}];
(* randomVector[n_] := Table[RandomInteger[] * 2 - 1, {k, n}]; *)
circulantMatrix[vector_] := Table[vector[[Mod[i - j, Length[vector]] + 1]], {j, 0, Length[vector] - 1}, {i, 0, Length[vector] - 1}];

eigenvalues = {};
eigenGroups = {};
For[i = 1, i <= repeat, i++,
    If[Mod[i, 10000] == 1, Print[i]];
    matrix = circulantMatrix[randomVector[n]];
    AppendTo[eigenGroups, Eigenvalues[matrix]];
];
eigenvalues = Flatten[eigenGroups];
Print[eigenvalues//Length];

evRealImag = Map[{Re[#], Im[#]}&, eigenvalues];
range = n;
graph = ListPlot[evRealImag, 
    AspectRatio -> 1, 
    PlotRange -> {{-range, range}, {-range, range}}, 
    Axes -> False];

outputFileName = "eigen_values" <> If[exportToPov, "_pov_", "_native_"] <> IntegerString[n] <> ".png";
outputFileName = FileNameJoin[{imageFolder, outputFileName}];
cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

If[exportToPov,
    If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
    POVRayRender[graph, 
        Method -> "Triangle", 
        OutputPath -> outputFileName, 
        ImageSize -> imageSize
        , RenderOptions -> "+A0.001 -J +UA"
    ];
    Run[cropCommand];
    ,
    Export[outputFileName, graph, ImageSize -> imageSize[[2]] ];
];

