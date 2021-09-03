#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 250 * {4, 3};
exportToPov = False;

n = 7;
repeat = 300 * 1000;
sigma = 0.003;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

(* randomVector[n_, sigma_] := Table[RandomInteger[] * 2 - 1 + RandomVariate[NormalDistribution[0, sigma]], {k, n}]; *)

(* randomVector[n_, sigma_] := Table[
    Mod[
        RandomVariate[NormalDistribution[0, sigma]]^3, 
    1], {k, n}];
 *)

(* randomVector[n_, sigma_] := Table[
    Mod[
        RandomVariate[LaplaceDistribution[0, sigma]], 
    1], {k, n}];
 *)

(* randomVector[n_, sigma_] := Table[
    Mod[
        RandomVariate[UniformDistribution[{0, 1}]], 
    1], {k, n}];
 *)

randomVector[n_, sigma_] := Table[
    Mod[
        RandomVariate[CauchyDistribution[0, 0.0003]], 
    1], {k, n}];


(* only beta distribution works. Both offset beta or original beta work *)
(* randomVector[n_, betaParam_] := Table[RandomVariate[BetaDistribution[betaParam, betaParam]] * 2 - 1, {k, n}]; *)

(* randomVector[n_, betaParam_] := Table[RandomVariate[BetaDistribution[betaParam, betaParam]], {k, n}]; *)

dft = {};
groups = {};
For[i = 1, i <= repeat, i++,
    If[Mod[i, 10000] == 0, Print[i]];
    v = randomVector[n, sigma];
    AppendTo[groups, Fourier[v]];
];
dft = Flatten[groups];
Print[dft//Length];

evRealImag = Map[{Re[#], Im[#]}&, dft];
range = Sqrt[n]/2.7;
pointSize = 0.7/Sqrt[repeat];
graph = ListPlot[evRealImag, 
    AspectRatio -> 1, 
    PlotRange -> {{-range, range}, {-range, range}}, 
    PlotStyle -> {PointSize[pointSize], Black},
    Axes -> False];

outputFileName = "dft" <> If[exportToPov, "_pov_", "_native_"] <> IntegerString[n] <> ".png";
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
Print["saved image to " <> outputFileName];
