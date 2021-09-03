#!/usr/bin/env wolframscript

imageFolder = "output";
imageSize = 250 * {4, 3};
exportToPov = False;

n = 7;
sigma = 0.001;
frameCount = 100;
repeatPerFrame = 10000;

(* Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];
 *)

(* randomVector[n_, sigma_] := Table[RandomInteger[] * 2 - 1 + RandomVariate[NormalDistribution[0, sigma]], {k, n}]; *)
(* randomVector[n_, sigma_] := Table[
    Mod[
        RandomVariate[NormalDistribution[0, sigma]]^3, 
    1] * 2 - 1, {k, n}];
 *)

(* only beta distribution works. Both offset beta or original beta work *)
(* randomVector[n_, betaParam_] := Table[RandomVariate[BetaDistribution[betaParam, betaParam]] * 2 - 1, {k, n}]; *)

randomVector[n_, betaParam_] := Table[RandomVariate[BetaDistribution[betaParam, betaParam]], {k, n}];

groups = {};
totalRepeat = frameCount * repeatPerFrame;

exportImage[repeat_, serialNumber_, frameFolder_] := Module[
    {
        i, v, dft, evRealImag, range, pointSize, graph, outputFolder, outputFileName, cropCommand
    },

    For[i = 1, i <= repeat, i++,
        v = randomVector[n, sigma];
        AppendTo[groups, Fourier[v]];
    ];

    dft = Flatten[groups];
    Print[dft//Length];

    evRealImag = Map[{Re[#], Im[#]}&, dft];
    range = Sqrt[n]/2.7;
    pointSize = 0.7/Sqrt[totalRepeat];
    (* pointSize = 0.7/Sqrt[1000*1000]; *)
    graph = ListPlot[evRealImag, 
        AspectRatio -> 1, 
        PlotRange -> {{-range, range}, {-range, range}}, 
        PlotStyle -> {PointSize[pointSize], Black},
        Axes -> False];

    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

    outputFileName = "animation_frame_" <> IntegerString[serialNumber, 10, 4] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];
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

];

exportAnimation[] := Module[
    {
        shape, gifFileName, frameFolder, gifFolder, frameIndex, delay, gifCommand
    },

    shape = "dft";
    gifFileName = shape 
        <> IntegerString[n]
        <> "_"
        <> IntegerString[frameCount]
        <> "_"
        <> IntegerString[repeatPerFrame];

    frameFolder = gifFileName <> "_frames";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    outputFolder = FileNameJoin[{imageFolder, frameFolder}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    For[frameIndex = 1, frameIndex <= frameCount, frameIndex++,
        Print["Frame #" <> IntegerString[frameIndex] <> " / " <> IntegerString[frameCount]];
        exportImage[repeatPerFrame, frameIndex, frameFolder];
    ];

    delay = If[frameCount >= 30, 3, Floor[500/frameCount]];
    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[delay] <> " -dispose 2 " <> outputFolder <> "\\*animation*.png " <> " -delay 300 " <> outputFolder <> "\\animation_frame_" <> IntegerString[frameCount, 10, 4] <> ".png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];

(*     mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
        <> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
        <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"]; *)
];

(* exportImage[10000, 1, "test"];
exportImage[10000, 2, "test"];
exportImage[10000, 3, "test"];
 *)
exportAnimation[];
