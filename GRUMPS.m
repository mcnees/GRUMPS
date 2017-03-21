(* ::Package:: *)

(*---------*)
(*Changelog*)
(*---------*)
(* March 21st, 2017 
  - Freshen up code for uploading to GitHub.
*)


(* September 12th
  - Added Einstein Tensor
*)


(*July 6th
	- Implemented ShowArray command, which combines all the show commands.
	- Fixed a bad call for the Ricci Scalar in the calculation of the Einstein tensor
	-
	- Incremented version to 0.1.5, changed date to July 2015
*)



(*June 30
	- For some reason, the assignment "userVol=Sqrt[(-1)^sig Det[metric]]//OurSimplify;" in the 
	  DefineSpacetime command was adding significant overhead. Changing it to Sqrt[Abs[Det[Metric]]]
      seems to have fixed this, though I don't know why the other one was so slow.
	- Should we set $Assumptions before things like userVol? Otherwise Simplify or OurSimplify
	  Can't replace things like Abs[Known pos quantity] with Known pos quantity. Trying to do this
	  by moving the relevant chunk of code higher up in DefSpacetime slows things down
	  by a few seconds.
	- Incremented version to 0.1.4
*)



(*June 29
	- Changed package name to GRUMPS ("General Relativity Using Multiple Processors and Symmetries").
	- Fixed volume element to be Sqrt[(-1)^sDet[metric]].
	- Tinkered with formatting in ShowSpacetime command. Put "lineEl" in a Panel with correct baseline so it doesn't wrap under the ds^2 part.
	- Put Changelog in reverse Chronological order; newest at the top.
	- Incremented version number to 0.1.3.
	- TO DO: Rewrite anything that displays components to not use Column. When using column the size of equations 
		gets reduced, and they are too small to comfortably read. Replace entries of Column with something like
		  Tensor Component = Panel[value of Tensor Component,BaselinePosition \[Rule] Baseline]
		Not totally clear how to do this.
*)


(* June 15
	- Updated version number to 0.1.2
	- Usage messages altered according to above.
	- ShowAll commands output a message if all components are zero.
	- Volume element calculated and exportd in DefSpacetime.
	- DefSpacetime displays a message of what has been exported, which prints an export message in the appropriate place
		if the user doesn't suppress DefSpacetime's output and always appropriately prints the export message if the user redefines spacetime.
		This implementation is the best solution I could come up with to avoid messages printing in those annoyoing windows, but I'm sure there is a better
		implementation.
	

*)


(*June 7:
	- Changed inverse metric name from metricName<>"Inverse" to metricName<>"Inv". Makes it a little shorter.
	- Updated version number to 0.1.1. From now on, any other change besides typos increments the version number by 0.0.1.
	- Updated the date to June 2015. 
	- Added newline (\n) after "Line Element:" in ShowSpacetime command, so "ds^2=..." starts on next line.
	- Removed clickable buttons from on-load message. The problems with text being printed out of order, etc, were
	  too annoying and not worth the hassle.
	- Updated and edited usage message for DefSpacetime. In general arguments of functions should be indicated in italics,
	  with one pair of curly braces around lists and two pairs of curly braces around arrays. The list of arguments for a
	  function should include one space after each comma. For example: DefSpacetime[{coords}, {{met}}, sig].
	- To Do: Update usage messages for other functions.
	- To Do: If all components of a tensor are zero, the associated "ShowAll" command should issue a statement like 
	  "All components of <Tensor Name> are zero."
	- To Do: Export Sqrt[(-1)^sDet[Subscript[g, \[Mu]\[Nu]]]] as metricName<>"Vol" or similar.
	- Question: Should DefSpacetime return a message like "Metric exported as metricName, inverse metric exported as metricNameInv."?
	- Question: Does it make sense to append metricName to the beginning (or end) of all exported quantities? For instance,
	  if the metric is named "g", export the Ricci Scalar as gRicciScalar?
*)



(* ::Section:: *)
(*Function Descriptions*)


BeginPackage["GRUMPS`"];

GRUMPSCommands::usage="GRUMPSCommands[] lists the commands provided by the GRUMPS package.";

GRUMPSConventions::usage="GRUMPSConventions[] lists the conventions used in the calculations.";

DefSpacetime::usage="DefSpacetime[{\!\(\*
StyleBox[\"coords\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\), {{\!\(\*
StyleBox[\"met\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"sig\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"{\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"assump\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] defines a coordinate chart on a spacetime. The coordinates are given by the list \!\(\*
StyleBox[\"coords\",\nFontSlant->\"Italic\"]\) and the components of the metric are given by the square array \!\(\*
StyleBox[\"met\",\nFontSlant->\"Italic\"]\), with metric signature \!\(\*
StyleBox[\"sig\",\nFontSlant->\"Italic\"]\). The optional argument \!\(\*
StyleBox[\"{\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"assump\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"}\",\nFontSlant->\"Italic\"]\) is a list of assumptions about the coordinates and the parameters in the metric. Coordinates and parameters are otherwise assumed to be real numbers. The optional argument \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\) is used to export the metric (\!\(\*
StyleBox[\"met\",\nFontSlant->\"Italic\"]\)), its inverse (\!\(\*
StyleBox[\"metInv\",\nFontSlant->\"Italic\"]\)), and the volume element (\!\(\*
StyleBox[\"metVol\",\nFontSlant->\"Italic\"]\))."; 
ShowSpacetime::usage= "ShowSpacetime[] displays the coordinates, metric, line element, and assumptions associated with the spacetime.";

ComputeChristoffel::usage="ComputeChristoffel[] outputs the Christoffel symbol for the defined spacetime to the Mathematica variable Christoffel.";

ComputeRiemann::usage= "ComputeRiemann[] outputs the completely covariant Riemann tensor for the defined spacetime to the Mathematica variable Riemann.";
ComputeRiemannUU::usage="ComputeRiemannUU[] outputs the (+2,-2) Riemann tensor for the defined spacetime to the Mathematica variable RiemannUU.";
ComputeRiemannSquared::usage="ComputeRiemannSquared[] outputs the Kretschmann scalar to the Mathematica variable RiemannSquared.";
ComputeDualRiemann::usage="ComputeRiemannDual[] outputs the completely coavariant dual of the Riemann Tensor for 4-dimensional spacetimes to the Mathematica variable DualRiemann.";
ComputeDualRiemannUU::usage="ComputeRiemannUU[] outputs the (+2,-2) dual Riemann tensor for the defined spacetime to the Mathematica variable DualRiemannUU.";
ComputeChernPontryagin::usage="ComputeChernPontryagin[] outputs the Chern-Pontryagin density to the Mathematica variable ChernPontryagin.";

ComputeRicci::usage= "ComputeRicci[] outputs the completely covariant Ricci tensor for the defined spacetime to the Mathematica variable Ricci.";
ComputeRicciScalar::usage="ComputeRicciScalar[] outputs the Ricci scalar for the defined spacetime to the Mathematica variable RicciScalar.";
ComputeRicciU::usage ="ComputeRicciU[] outputs (+1,-1) Ricci tensor for the defined spacetime to the Mathematica variable RicciU.";
ComputeRicciSquared::usage="ComputeRicciSquared[] outputs Ricci squared for the defined spacetime to the Mathematica variable RicciSquared."
ComputeEinstein::usage= "ComputeEinstein[] outputs the completely covariant Einstein tensor for the defined spacetime to the Mathematica variable Einstein.";

ComputeWeyl::usage= "ComputeWeyl[] outputs the completely covariant Weyl tensor for the defined spacetime to the Mathematica variable Weyl.";
ComputeWeylUU::usage="ComputeRiemannUU[] outputs the (+2,-2) Weyl tensor for the defined spacetime to the Mathematica variable WeylUU.";
ComputeWeylSquared::usage= "ComputeWeylSquared[] outputs Weyl squared  to the Mathematica variable WeylSquared."

ComputeCotton::usage="ComputeCotton[] outputs the completely covariant Cotton tensor for the defined spacetime to the Mathematica variable Cotton."

ComputeSchouten::usage="ComputeSchouten[] outputs the completely covariant Schouten tensor for the defined spacetime to the Mathematica variable Schouten."

ShowArray::usage="ShowArray['\!\(\*
StyleBox[\"arrayName\",\nFontSlant->\"Italic\"]\)'] prints all non-zero components of the array corresponding to '\!\(\*
StyleBox[\"arrayName\",\nFontSlant->\"Italic\"]\)'. ShowArray['\!\(\*
StyleBox[\"arrayName\",\nFontSlant->\"Italic\"]\)',{\!\(\*
StyleBox[\"indices\",\nFontSlant->\"Italic\"]\)}] shows the component with indices given by {\!\(\*
StyleBox[\"indices\",\nFontSlant->\"Italic\"]\)}." 


Begin["Private`"];


(* ::Section:: *)
(*Declaring Flags*)


(* ::Text:: *)
(*Declare flags that tell us whether certain objects have or have not been evaluated:*)


LaunchKernels[];
Print[Column[{"GRUMPS: General Relativity Using Multiple Processors and Symmetries","v0.1.6 March 2017",Style[Hyperlink["http://jacobi.luc.edu/GRUMPS.html"]],Row[{">> ",Style[ToString[$KernelCount],Blue, Bold]," kernels loaded."}],Row[{">> For a list of functions and their usage evaluate ",Style["GRUMPSCommands[]",Blue]}],Row[{">> To define a spacetime start with the command ",Style["DefSpacetime[]",Blue]}],Row[{">> To see a list of conventions and definitions use the command ",Style["GRUMPSConventions[]",Blue]}],""}]]
ResetFlags:=
($GRUMPSVersion=0.1.6;
$SpacetimeSet=False;
$ChristoffelEvaluated= False;
$RiemannEvaluated = False;
$RiemannUUEvaluated=False;
$RiemannSquaredEvaluated=False;
$DualRiemannEvaluated=False;
$DualRiemannUUEvaluated=False;
$ChernPontryaginEvaluated=False;
$RicciEvaluated = False;
$RicciScalarEvaluated = False;
$EinsteinEvaluated = False;
$RicciUEvaluated=False;
$RicciSquaredEvaluated = False;
$EinsteinEvaluated=False;
$WeylEvaluated=False;
$WeylUUEvaluated = False;
$WeylSquaredEvaluated=False;
$CottonEvaluated=False;
$SchoutenEvaluated=False;)

ResetFlags;

Clear[Global`Christoffel,Global`Riemann,Global`RiemannUU,Global`DualRiemann,Global`DualRiemannUU, Global`RiemannSquared,Global`Ricci,Global`RicciScalar,Global`Einstein,Global`RicciU,Global`RicciSquared,Global`Einstein,Global`Weyl,Global`WeylUU,Global`WeylSquared,Global`Cotton,Global`Schouten];
Protect[Global`Christoffel,Global`Riemann,Global`RiemannUU,Global`DualRiemann, Global`DualRiemannUU, Global`RiemannSquared,Global`Ricci,Global`RicciScalar,Global`Einstein,Global`RicciU,Global`RicciSquared,Global`Einstein,Global`Weyl,Global`WeylUU,Global`WeylSquared,Global`Cotton,Global`Schouten];


(* ::Text:: *)
(*Note : Need a way to globally set TimeConstraint -> Infinity for all Simplify Operations. Define our own Simplify function which just sets an infinite TimeConstraint. This *may* cause longer evaluations for some quantities, as Mathematica continues to look for simpler forms. On the other hand, subsequent simplifications of other quantities might be faster because they start with simpler inputs.*)


(*OurSimplify[X_]:= Simplify[X, TimeConstraint->Infinity]*)
OurSimplify[X_]:= Simplify[X]
Protect[OurSimplify];
DistributeDefinitions[OurSimplify];


(* ::Section::Closed:: *)
(*GRUMPSCommands*)


GRUMPSCommands[]:= (CellPrint@ExpressionCell["Click on a command for a detailed usage message.","Print",PrivateCellOptions->{"EvaluationUnmatchedStyle"->{}}];Information["GRUMPS`*",LongForm->False]);
Protect[GRUMPSCommands];


(* ::Section::Closed:: *)
(*Conventions*)


GRUMPSConventions[]:= CellPrint@
 ExpressionCell[
  Column[{"Some important conventions used by GRUMPS", 
    "Riemann tensor:  \!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\
\[Lambda]\)], \(\[Mu]\[Sigma]\[Nu]\)]\) = \!\(\*SubscriptBox[\(\
\[PartialD]\), \(\[Sigma]\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\
\[CapitalGamma]\), \(\[Lambda]\)], \
\(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(\[PartialD]\), \
\(\[Nu]\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\
\[Lambda]\)], \(\[Mu]\[Sigma]\)]\)+\!\(\*SubscriptBox[SuperscriptBox[\
\(\[CapitalGamma]\), \(\[Lambda]\)], \
\(\[Sigma]\[Kappa]\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\
\[CapitalGamma]\), \(\[Kappa]\)], \
\(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\
\), \(\[Lambda]\)], \
\(\[Nu]\[Kappa]\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\
\[CapitalGamma]\), \(\[Kappa]\)], \(\[Mu]\[Sigma]\)]\)", 
    "Ricci tensor:  \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) = \
\!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[Lambda]\)], \(\[Mu]\
\[Lambda]\[Nu]\)]\)", 
    "Dual Riemann tensor: \!\(\*SubscriptBox[OverscriptBox[\(R\), \
\(~\)], \(\[Lambda]\[Mu]\[Sigma]\[Nu]\)]\) = \!\(\*FractionBox[\(1\), \
\(2!\)]\)\!\(\*SubscriptBox[\(\[Epsilon]\), \(\[Lambda]\[Mu]\[Alpha]\
\[Beta]\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(R\), \
\(\[Alpha]\[Beta]\)], \(\[Sigma]\[Nu]\)]\)", 
    "Covariant Levi-Civita tensor: \!\(\*SubscriptBox[\(\[Epsilon]\), \
\(012  \[Ellipsis]n\)]\) = +1 \
\!\(\*SqrtBox[\(\*SuperscriptBox[\((\(-1\))\), \(s\)] det \
\((\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)])\)\)]\)", 
    "Contravariant Levi-Civita tensor: \!\(\*SuperscriptBox[\(\
\[Epsilon]\), \(012  \[Ellipsis]n\)]\) = \!\(\*FractionBox[\((\(-1\) \
\*SuperscriptBox[\()\), \(s\)]\), \
SqrtBox[\(\*SuperscriptBox[\((\(-1\))\), \(s\)] det \
\((\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)])\)\)]]\)"}, Spacings -> 1], 
  "Print", PrivateCellOptions -> {"EvaluationUnmatchedStyle" -> {}}];
Protect[GRUMPSConventions];


(* ::Section::Closed:: *)
(*Define Spacetime*)


DefSpacetime[coords_,metric_,sig_,assumptions_:{},metricName_:"Metric"]:=
If[$SpacetimeSet,
	expString=DialogInput[Column[{"You have already defined the spacetime. Do you want to start over and define a new spacetime?",\
		Row[{CancelButton["No",DialogReturn[]],
			DefaultButton["Yes",DialogReturn[ResetFlags;DefSpacetime[coords,metric,sig,assumptions,metricName]]]}]}],WindowTitle-> "Really do this?"];Print[expString],
	Module[{coordsTest=False,dimTest=False,symTest=False,assumptionsTest=False,sigTest=False,nameTest=False},
		If[coords//VectorQ,coordsTest=True,Message[DefSpacetime::CoordsError] ];
		If[metric//SymmetricMatrixQ,symTest=True,Message[DefSpacetime::MetricSymmetry]];
		If[Length[coords]== Length[metric]&& Length[coords]== Length[metric[[1]]],dimTest=True,Message[DefSpacetime::InputAgreement]];
		If[ListQ[assumptions],assumptionsTest=True,Message[DefSpacetime::AssumptionsError]];
		If[IntegerQ[sig],sigTest=True,Message[DefSpacetime::SignatureError]];
		If[StringQ[metricName],nameTest=True,Message[DefSpacetime::NameError]];
		If[coordsTest&& symTest&&dimTest&&sigTest&&assumptionsTest&&nameTest,
			reservedCoords=coords ;
			diffCoords=Map[ToExpression["d"<>ToString[#]]&,reservedCoords];
			reservedDim=Length[coords]; 
			reservedMetric=metric;
			reservedInverseMetric=Inverse[metric]//OurSimplify;
			
			With[{
				userMetricString="Global`"<>metricName,
				userInverseMetricString="Global`"<>metricName<>"Inv",
				userVolString="Global`"<>metricName<>"Vol"
				},
				Clear[userMetricString,userInverseMetricString,userVolString];			
				];

			With[{
				userMetric=Symbol@@{"Global`"<>metricName},
				ourMetric=reservedMetric,
				userInverseMetric=Symbol@@{"Global`"<>metricName<>"Inv"},
				ourInverseMetric=reservedInverseMetric,
				userVol=Symbol@@{"Global`"<>metricName<>"Vol"}
				},
				userMetric=ourMetric;
				userInverseMetric=ourInverseMetric;	
				(*userVol=Sqrt[(-1)^sig Det[metric]]//OurSimplify;*)	
				userVol=Sqrt[Abs[Det[metric]]]//OurSimplify;						
				];

			lineEl=0;
			diffEls={};
			For[i=1,i<=reservedDim,i++,For[j=1,j<=i,j++,diffEls=Append[diffEls,diffCoords[[j]] diffCoords[[i]]];]];
			For[i=1,i<= reservedDim,i++,For[j=1,j<= reservedDim,j++,lineEl=lineEl+reservedMetric[[i,j]] diffCoords[[i]] diffCoords[[j]];]];
			reservedlineEl=Collect[lineEl,diffEls,OurSimplify];

			inputParams=Complement[Variables[lineEl],diffCoords];
			specialFunctions=Map[If[StringMatchQ[ToString[#],{"Cos*","Sec*","Sin*","Csc*","Tan*","Cot*"}],#]&,inputParams];
			inputParams=Complement[Union[inputParams,coords],specialFunctions];
			userParams= Map[If[StringMatchQ[ToString[assumptions],"*"<>ToString[#]<>"*"],#]&,inputParams];
			omittedParams=Complement[inputParams,userParams];
			ourAssumptions=Map[Element[# ,Reals] &,omittedParams];
			ourAssumptions=Union[ourAssumptions,assumptions];
			$Assumptions=ourAssumptions;
			
			LeviCivita= Sqrt[Abs[Det[reservedMetric]]]*LeviCivitaTensor[reservedDim];
			ContraLeviCivita=(-1)^sig(Sqrt[Abs[Det[reservedMetric]]])^-1 LeviCivitaTensor[reservedDim];

			ChristoffelSyms={{{1,3,2},1}};
			ChristoffelPerms=SymmetrizedIndependentComponents[{reservedDim,reservedDim,reservedDim},ChristoffelSyms];
			RiemannSyms={{{3,4,1,2},1},{{1,2,4,3},-1},{{2,1,3,4},-1}};
			RiemannPerms=SymmetrizedIndependentComponents[{reservedDim,reservedDim,reservedDim,reservedDim},RiemannSyms];
			RiemannUUSyms={{{1,2,4,3},-1},{{2,1,3,4},-1}};
			RiemannUUPerms=SymmetrizedIndependentComponents[{reservedDim,reservedDim,reservedDim,reservedDim},RiemannUUSyms];
			RiemannUUDepCompLengths = Length[SymmetrizedDependentComponents[#,RiemannUUSyms]]&/@RiemannUUPerms;
			RicciSyms={{{2,1},1}};
			RicciPerms=SymmetrizedIndependentComponents[{reservedDim,reservedDim},RicciSyms];
			CottonSyms={{{1,3,2},-1}};
			CottonPerms=SymmetrizedIndependentComponents[{reservedDim,reservedDim,reservedDim},CottonSyms];
			$SpacetimeSet=True;
			DistributeDefinitions[LeviCivita,ContraLeviCivita,reservedCoords,reservedMetric,reservedInverseMetric,reservedDim,ChristoffelPerms,RiemannPerms,RiemannUUPerms,RiemannUUDepCompLengths,RicciPerms,CottonPerms,lineEl];
			exportString="Metric exported to "<> metricName<>".\nInverse metric exported to "<> metricName<>"Inv.\nVolume element exported to "<>metricName<>"Vol."]]]

DefSpacetime::CoordsError="First argument must be a list of coordinates.";
DefSpacetime::MetricSymmetry="Second argument must be a symmetric square array.";
DefSpacetime::InputAgreement="Coordinate and metric dimension do not agree.";
DefSpacetime::AssumptionsError="Assumptions must be input as a list.";
DefSpacetime::SignatureError="Metric signature must be a positive integer.";
DefSpacetime::NameError="Desired metric name must be entered as a string.";


ShowSpacetime[]:= 
If[$SpacetimeSet, 
	Print["Coordinates: ", reservedCoords,"\n",
"Metric:\n","  ",Module[{zeroPerms,nonZeroPerms}, zeroPerms=Map[If[reservedMetric[[#[[1]],#[[2]]]]==0,#]&,RicciPerms];nonZeroPerms=Complement[RicciPerms,zeroPerms];
Column[Row[{Subscript["g",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]]," = ",reservedMetric[[#[[1]],#[[2]]]]}]&/@nonZeroPerms,Spacings-> 0.75]],"\n",
"Line Element:\n","  ","\!\(\*SuperscriptBox[\(ds\), \(2\)]\) = ",Pane[reservedlineEl,BaselinePosition->Baseline],"\n","
Assumptions: ",$Assumptions],
Message[ShowSpacetime::NoSpacetime]] ;

ShowSpacetime::NoSpacetime="Spacetime has not been defined. Use DefSpacetime[coords,metric,signature] to define spacetime.";



Protect[DefSpacetime,ShowSpacetime];


(* ::Section::Closed:: *)
(*Calculate and Display Christoffel Symbol*)


ComputeChristoffel[]:=
Module[{ChristoffelTimingResults}, 
	If[$SpacetimeSet==False,
		Message[ComputeChristoffel::NoSpacetime];Abort[];, 
		If[$ChristoffelEvaluated,
			Print["You have already calculated the Christoffel symbol for the defined spacetime."],
			(Print["Computing components of the Christoffel symbol."];
			ChristoffelTimingResults = AbsoluteTiming[ reservedChristoffel= SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]]}->1/2 Sum[reservedInverseMetric[[#[[1]],Dummy1]]*(D[reservedMetric[[Dummy1,#[[3]]]],reservedCoords[[#[[2]]]]]+D[reservedMetric[[#[[2]],Dummy1]],reservedCoords[[#[[3]]]]]-D[reservedMetric[[#[[2]],#[[3]]]],reservedCoords[[Dummy1]]]),{Dummy1,1,reservedDim}]//OurSimplify)&,ChristoffelPerms]],{reservedDim,reservedDim,reservedDim},ChristoffelSyms];];
			$ChristoffelEvaluated = True;
			Print[">> Computed " <> ToString[Length[ChristoffelPerms]]<> " independent components of the Christoffel symbol in "<>ToString[ChristoffelTimingResults[[1]]]<>" seconds."]);
			Unprotect[Global`Christoffel];
			Global`Christoffel=reservedChristoffel;
			Protect[Global`Christoffel];
			DistributeDefinitions[reservedChristoffel];]]];

ComputeChristoffel::NoSpacetime="Spacetime has not been defined. Use DefSpacetime[coords_,metric_] to define spacetime.";



ShowChristoffel[indexList_]/;
If[Length[indexList]!=3,Message[ShowChristoffel::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]],True,Message[ShowChristoffel::NotIndices];False]]:=
	If[$ChristoffelEvaluated,
		Print[ Row[{Subscript[Superscript["\[CapitalGamma]",Style[reservedCoords[[indexList[[1]]]],Blue]],Style[ToString[reservedCoords[[indexList[[2]]]]]<>ToString[reservedCoords[[indexList[[3]]]]],Red]]," = ",reservedChristoffel[[indexList[[1]],indexList[[2]],indexList[[3]]]]}]],
		ComputeChristoffel[];
		ShowChristoffel[indexList]];

ShowChristoffel::NotIndices="The arguments given to this function must be a list of numbers indicating the positions of each coordinate.";
ShowChristoffel::IndicesLength="Index list must be length three.";


ShowCoordChristoffel[coordList_]/;
If[Length[coordList]!=3,Message[ShowCoordChristoffel::CoordsLength],If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]],True,Message[ShowCoordChristoffel::NotCoordinates];False]]:= 
	If[$ChristoffelEvaluated,
		Print[ Row[{Subscript[Superscript["\[CapitalGamma]",Style[coordList[[1]],Blue]],Style[ToString[coordList[[2]]]<>ToString[coordList[[3]]],Red]]," = ",reservedChristoffel[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]]]]}]],
		ComputeChristoffel[];
		ShowCoordChristoffel[coordList]];

ShowCoordChristoffel::NotCoordinates="The arguments given to this function must be a list of three coordinates.";
ShowCoordChristoffel::CoordsLength="Coordinate list must be length three.";


ShowAllChristoffel[]:=
If[$ChristoffelEvaluated,
	Module[{zeroPerms,nonZeroPerms},
			zeroPerms=Map[If[reservedChristoffel[[#[[1]],#[[2]],#[[3]]]]==0,#]&,ChristoffelPerms];
			nonZeroPerms=Complement[ChristoffelPerms,zeroPerms];
			Print[">> Printing non-zero components of the Christoffel symbol:"]; 
			If[nonZeroPerms=={}, Print["All components of the Christoffel symbol are zero."]];
			Print[Column[Row[{Subscript[Superscript["\[CapitalGamma]",Style[reservedCoords[[#[[1]]]],Blue]],Style[ToString[reservedCoords[[#[[2]]]]]<>ToString[reservedCoords[[#[[3]]]]],Red]]," = ",reservedChristoffel[[#[[1]],#[[2]],#[[3]]]]}]&/@nonZeroPerms]]],
			ComputeChristoffel[];
			ShowAllChristoffel[];]


Protect[ComputeChristoffel];


(* ::Section::Closed:: *)
(*Calculate and Display Riemann Tensor*)


ComputeRiemann[]:=
Module[{RiemannTiming},
	If[$RiemannEvaluated,
		Print["You have already calculated the Riemann tensor."], 
		(If[$ChristoffelEvaluated,
			Print["Computing components of the Riemann tensor."]; 
			RiemannTiming=AbsoluteTiming[
				reservedRiemann=SymmetrizedArray[
									Parallelize[
										Map[({#[[1]],#[[2]],#[[3]],#[[4]]}-> Sum[reservedMetric[[#[[1]],Dummy2]](D[reservedChristoffel[[Dummy2,#[[2]],#[[4]]]],reservedCoords[[#[[3]]]]]-D[reservedChristoffel[[Dummy2,#[[2]],#[[3]]]],reservedCoords[[#[[4]]]]]+Sum[reservedChristoffel[[Dummy1,#[[2]],#[[4]]]]reservedChristoffel[[Dummy2,Dummy1,#[[3]]]]-reservedChristoffel[[Dummy1,#[[2]],#[[3]]]]reservedChristoffel[[Dummy2,Dummy1,#[[4]]]],{Dummy1,1,reservedDim}]),{Dummy2,1,reservedDim}]//OurSimplify)&,RiemannPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannSyms];];
			$RiemannEvaluated=True;
			Print[">> Computed "<>ToString[Length[RiemannPerms]]<>" independent components of the Riemann tensor in "<>ToString[RiemannTiming[[1]]]<>" seconds."];,
			ComputeChristoffel[];
			ComputeRiemann[];];
		Unprotect[Global`Riemann];
		Global`Riemann=reservedRiemann;
		Protect[Global`Riemann];
		DistributeDefinitions[reservedRiemann];)]];


ShowRiemann[indexList_]/;
If[Length[indexList]!=4,Message[ShowRiemann::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[4]]<= Length[reservedCoords]],True,Message[ShowRiemann::NotIndices];False]]:=
	If[$RiemannEvaluated,
		Print[ Row[{Subscript["R",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]]<>ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedRiemann[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeRiemann[];
		ShowRiemann[indexList]];

ShowRiemann::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowRiemann::IndicesLength="Index list must be length four.";


ShowCoordRiemann[coordList_]/;
If[Length[coordList]!=4,Message[ShowCoordRiemann::CoordsLength]False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordRiemann::NotCoordinates];False]]:= 
	If[$RiemannEvaluated,
		Print[ Row[{Subscript["R",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]]<>ToString[coordList[[3]]]<>ToString[coordList[[3]]],Red]]," = ",reservedRiemann[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeRiemann[];
		ShowCoordRiemann[coordList]];

ShowCoordRiemann::NotCoordinates="The arguments given to this function must be the names of coordinates.";ShowRiemann::IndicesLength="Index list must be length four.";
ShowCoordRiemann::CoordsLength="Coordinate list must be length four.";


ShowAllRiemann[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$RiemannEvaluated,
			zeroPerms=Map[If[reservedRiemann[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannPerms];
			nonZeroPerms=Complement[RiemannPerms,zeroPerms];
			Print[">> Printing non-zero components of the Riemann tensor:"];
			If[nonZeroPerms=={}, Print["All components of the Riemann tensor are zero."]];
			Print[Column[Row[{Subscript["R",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]]<>ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedRiemann[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeRiemann[];
			ShowAllRiemann[]]]


Protect[ComputeRiemann];


(* ::Section::Closed:: *)
(*(+2,-2) Riemann Tensor*)


ComputeRiemannUU[]:= 
Module[{RiemannUUTiming},
	If[$RiemannUUEvaluated,
		Print["You have already calculated the (+2,-2) Riemann tensor."], 
		(If[$RiemannEvaluated,
			Print["Computing components of the (+2,-2) Riemann tensor."]; 
			RiemannUUTiming=AbsoluteTiming[
			reservedRiemannUU=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]],#[[4]]}-> Sum[(reservedInverseMetric[[#[[1]],Dummy1]]reservedInverseMetric[[#[[2]],Dummy2]]-reservedInverseMetric[[#[[1]],Dummy2]]reservedInverseMetric[[#[[2]],Dummy1]])reservedRiemann[[Dummy1,Dummy2,#[[3]],#[[4]]]],{Dummy1,1,reservedDim},{Dummy2,1,Dummy1}]//OurSimplify)&,RiemannUUPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannUUSyms];];
			$RiemannUUEvaluated=True;
			Print[">> Computed "<>ToString[Length[RiemannUUPerms]]<>" independent components of the (+2,-2) Riemann tensor in "<>ToString[RiemannUUTiming[[1]]]<>" seconds."],
		ComputeRiemann[];
		ComputeRiemannUU[];]
		Unprotect[Global`RiemannUU];
		Global`RiemannUU=reservedRiemannUU;
		Protect[Global`RiemannUU];
		DistributeDefinitions[reservedRiemannUU];)]]


ShowRiemannUU[indexList_]/;
If[Length[indexList]!=4,Message[ShowRiemUU::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[4]]<= Length[reservedCoords]],True,Message[ShowRiemUU::NotIndices];False]]:=
	If[$RiemannUUEvaluated,
		Print[ Row[{Subscript[Superscript["R",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]],Style[ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedRiemannUU[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeRiemannUU[];
		ShowRiemannUU[indexList]];

ShowRiemUU::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowRiemUU::IndicesLength="Index list must be length four.";


ShowCoordRiemannUU[coordList_]/;
If[Length[coordList]!=4,Message[ShowCoordRiemUU::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordRiemUU::NotCoordinates];False]]:= 
	If[$RiemannUUEvaluated,
		Print[ Row[{Subscript[Superscript["R",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]],Style[ToString[coordList[[3]]]<>ToString[coordList[[4]]],Red]]," = ",reservedRiemannUU[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeRiemannUU[];
		ShowCoordRiemannUU[coordList]];

ShowCoordRiemUU::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordRiemUU::CoordsLength="Coordinate list must be length four.";


ShowAllRiemannUU[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$RiemannUUEvaluated,
			zeroPerms=Map[If[reservedRiemannUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannUUPerms];
			nonZeroPerms=Complement[RiemannUUPerms,zeroPerms];
			Print[">> Printing non-zero components of the (+2,-2) Riemann tensor:"];
			If[nonZeroPerms=={}, Print["All components of the (+2,-2) Riemann tensor are zero."]];
			Print[Column[Row[{Subscript[Superscript["R",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]],Style[ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedRiemannUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeRiemannUU[];
			ShowAllRiemannUU[]]]


Protect[ComputeRiemannUU];


(* ::Section::Closed:: *)
(*Kretschmann Scalar*)


ComputeRiemannSquared[]:= 
Module[{KretschmannTiming},
	If[$RiemannSquaredEvaluated,
		Print["You have already calculated the Kretschmann scalar."],
		(If[$RiemannUUEvaluated,
			Print["Computing the Kretschmann scalar."]; 
			KretschmannTiming=AbsoluteTiming[reservedRiemannSquared=Total[reservedRiemannUU[[RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]],RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]]]]reservedRiemannUU[[RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]],RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]]]]*RiemannUUDepCompLengths[[#]]&/@Range[Length[RiemannUUPerms]]]//OurSimplify];
			$RiemannSquaredEvaluated=True;
			Print[">> Computed the Kretschmann scalar in "<>ToString[KretschmannTiming[[1]]]<>" seconds."],
			ComputeRiemannUU[];
			ComputeRiemannSquared[];]
		Unprotect[Global`RiemannSquared];
		Global`RiemannSquared=reservedRiemannSquared;
		Protect[Global`RiemannSquared];
		DistributeDefinitions[reservedRiemannSquared];)]]


Protect[ComputeRiemannSquared];


(* ::Section::Closed:: *)
(*Calculate and Display Dual Riemann Tensor*)


ComputeDualRiemann[]:=
If[reservedDim==4,
	Module[{DualRiemannTiming},
		If[$DualRiemannEvaluated,
			Print["You have already calculated the dual Riemann tensor."], 
			(If[$RiemannUUEvaluated,
				Print["Computing components of the dual Riemann tensor."]; 
				DualRiemannTiming=AbsoluteTiming[
					reservedDualRiemann=SymmetrizedArray[
										Parallelize[
											Map[({#[[1]],#[[2]],#[[3]],#[[4]]}-> Sum[LeviCivita[[#[[1]],#[[2]],a,b]]reservedRiemannUU[[a,b,#[[3]],#[[4]]]],{a,1,reservedDim},{b,1,a}]//OurSimplify)&,RiemannUUPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannUUSyms];];
				$DualRiemannEvaluated=True;
				Print[">> Computed "<>ToString[Length[RiemannUUPerms]]<>" independent components of the dual Riemann tensor in "<>ToString[DualRiemannTiming[[1]]]<>" seconds."];,
				ComputeRiemannUU[];
				ComputeDualRiemann[];];
			Unprotect[Global`DualRiemann];
			Global`DualRiemann=reservedDualRiemann;
			Protect[Global`DualRiemann];
			DistributeDefinitions[reservedDualRiemann];)]],
		Print["The dual Riemann tensor is only implemented for four dimensional spacetimes."]];


ShowDualRiemann[indexList_]/;
If[Length[indexList]!=4,Message[ShowDualRiemann::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[4]]<= Length[reservedCoords]],True,Message[ShowDualRiemann::NotIndices];False]]:=
	If[$DualRiemannEvaluated,
		Print[ Row[{Subscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]]<>ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedDualRiemann[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeDualRiemann[];
		ShowDualRiemann[indexList]];

ShowDualRiemann::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowDualRiemann::IndicesLength="Index list must be length four.";


ShowCoordDualRiemann[coordList_]/;
If[Length[coordList]!= 4,Message[ShowCoordDualRiemann::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordDualRiemann::NotCoordinates];False]]:= 
	If[$DualRiemannEvaluated,
		Print[ Row[{Subscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]]<>ToString[coordList[[3]]]<>ToString[coordList[[4]]],Red]]," = ",reservedDualRiemann[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeDualRiemann[];
		ShowCoordDualRiemann[coordList]];

ShowCoordDualRiemann::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordDualRiemann::CoordsLength="Coordinate list must be length four.";


ShowAllDualRiemann[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$DualRiemannEvaluated,
			zeroPerms=Map[If[reservedDualRiemann[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannUUPerms];
			nonZeroPerms=Complement[RiemannUUPerms,zeroPerms];
			Print[">> Printing non-zero components of the dual Riemann tensor:"];
			If[nonZeroPerms=={}, Print["All components of the dual Riemann tensor are zero."]];
			Print[Column[Row[{Subscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]]<>ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedDualRiemann[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeDualRiemann[];
			ShowAllDualRiemann[]]]


Protect[ComputeRiemann];


(* ::Section::Closed:: *)
(*(+2,-2) Dual Riemann Tensor*)


ComputeDualRiemannUU[]:= 
If[reservedDim==4,
	Module[{DualRiemannUUTiming},
		If[$DualRiemannUUEvaluated,
			Print["You have already calculated the (+2,-2) Dual Riemann tensor."], 
			(If[$RiemannEvaluated,
				Print["Computing components of the (+2,-2) Dual Riemann tensor."]; 
				DualRiemannUUTiming=AbsoluteTiming[
				reservedDualRiemannUU=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]],#[[4]]}-> Sum[ContraLeviCivita[[#[[1]],#[[2]],a,b]]reservedRiemann[[a,b,#[[3]],#[[4]]]],{a,1,reservedDim},{b,1,a}]//OurSimplify)&,RiemannUUPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannUUSyms];];
				$DualRiemannUUEvaluated=True;
				Print[">> Computed "<>ToString[Length[RiemannUUPerms]]<>" independent components of the (+2,-2) Dual Riemann tensor in "<>ToString[DualRiemannUUTiming[[1]]]<>" seconds."],
			ComputeRiemann[];
			ComputeDualRiemannUU[];]
			Unprotect[Global`DualRiemannUU];
			Global`DualRiemannUU=reservedDualRiemannUU;
			Protect[Global`DualRiemannUU];
			DistributeDefinitions[Global`DualRiemannUU];)]],
	Print["The dual Riemann tensor is only implemented for four dimensional spacetimes."]]


ShowDualRiemannUU[indexList_]/;
If[Length[indexList]!=4,Message[ShowDualRiemannUU::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]],True,Message[ShowDualRiemannUU::NotIndices];False]]:=
	If[$DualRiemannUUEvaluated,
		Print[ Row[{Subscript[Superscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]],Style[ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedDualRiemannUU[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeDualRiemannUU[];
		ShowDualRiemannUU[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]];

ShowDualRiemannUU::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowDualRiemannUU::IndicesLength="Index list must be length four.";


ShowCoordDualRiemannUU[coordList_]/;
If[Length[coordList]!= 4,Message[ShowCoordDualRiemannUU::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordDualRiemannUU::NotCoordinates];False]]:= 
	If[$DualRiemannUUEvaluated,
		Print[ Row[{Subscript[Superscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]],Style[ToString[coordList[[3]]]<>ToString[coordList[[4]]],Red]]," = ",reservedDualRiemannUU[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeDualRiemannUU[];
		ShowCoordDualRiemannUU[coordList]];

ShowCoordDualRiemannUU::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordDualRiemannUU::CoordsLength="Coordinate list must be length four.";


ShowAllDualRiemannUU[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$DualRiemannUUEvaluated,
			zeroPerms=Map[If[reservedDualRiemannUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannUUPerms];
			nonZeroPerms=Complement[RiemannUUPerms,zeroPerms];
			Print[">> Printing non-zero components of the (+2,-2) Dual Riemann tensor:"];
			If[nonZeroPerms=={}, Print["All components of the (+2,-2) Dual Riemann tensor are zero."]];
			Print[Column[Row[{Subscript[Superscript["\!\(\*OverscriptBox[\(R\), \(~\)]\)",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]],Style[ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedDualRiemannUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeDualRiemannUU[];
			ShowAllDualRiemannUU[]]]


Protect[ComputeRiemannUU];


(* ::Section::Closed:: *)
(*Chern-Pontryagin Density*)


ComputeChernPontryagin[]:= 
Module[{ChernPontryaginTiming},
	If[$ChernPontryaginEvaluated,
		Print["You have already calculated the Chern-Pontryagin density."],
		(If[$RiemannUUEvaluated,
			If[$DualRiemannUUEvaluated,
				Print["Computing the Chern-Pontryagin density."]; 
			    ChernPontryaginTiming=AbsoluteTiming[reservedChernPontryagin=Total[reservedDualRiemannUU[[RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]],RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]]]]reservedRiemannUU[[RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]],RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]]]]*RiemannUUDepCompLengths[[#]]&/@Range[Length[RiemannUUPerms]]]//OurSimplify];
				$ChernPontryaginEvaluated=True;
				Print[">> Computed the Chern-Pontryagin density in "<>ToString[ChernPontryaginTiming[[1]]]<>" seconds."],\
				ComputeDualRiemannUU[];
				ComputeChernPontryagin[];],
			ComputeRiemannUU[];
			ComputeChernPontryagin[];]
		Unprotect[Global`ChernPontryagin];
		Global`ChernPontryagin=reservedChernPontryagin;
		Protect[Global`ChernPontryagin];
		DistributeDefinitions[reservedChernPontryagin];)]]


Protect[ComputeChernPontryagin];


(* ::Section::Closed:: *)
(*Calculate and Display Ricci Tensor*)


ComputeRicci[]:= 
Module[{RicciTiming},
	If[$RicciEvaluated,
		Print["You have already calculated the Ricci tensor."],
		(If[$RiemannEvaluated,
			Print["Computing components of the Ricci tensor."];
				RicciTiming=AbsoluteTiming[
					reservedRicci=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]]}-> Sum[reservedInverseMetric[[Dummy1,Dummy2]]reservedRiemann[[Dummy1,#[[1]],Dummy2,#[[2]]]],{Dummy1,1,reservedDim},{Dummy2,1,reservedDim}]//OurSimplify)&,RicciPerms]],{reservedDim,reservedDim},RicciSyms];];$RicciEvaluated=True;
				Print[">> Computed "<>ToString[Length[RicciPerms]]<>" independent components of the Ricci tensor in "<>ToString[RicciTiming[[1]]]<>" seconds."],
			ComputeRiemann[];
			ComputeRicci[];];
		Unprotect[Global`Ricci];
		Global`Ricci=reservedRicci;
		Protect[Global`Ricci];
		DistributeDefinitions[reservedRicci];)]];



ShowRicci[indexList_]/;
If[Length[indexList]!=2,Message[ShowRicci::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]],True,Message[ShowRicci::NotIndices];False]]:=
	If[$RicciEvaluated,
		Print[ Row[{Subscript["R",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]]," = ",reservedRicci[[indexList[[1]],indexList[[2]]]]}]],
		ComputeRicci[];
		ShowRicci[indexList]];

ShowRicci::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowRicci::IndicesLength="Index list must be length two.";


ShowCoordRicci[coordList_]/;
If[Length[coordList]!=2,Message[ShowCoordRicci::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]],True,Message[ShowCoordRicci::NotCoordinates];False]]:= 
	If[$RicciEvaluated,
		Print[ Row[{Subscript["R",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]]," = ",reservedRicci[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]]]]}]],
		ComputeRicci[];
		ShowCoordRicci[coordList]];

ShowCoordRicci::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordRicci::CoordsLength="Coordinate list must be length two.";


ShowAllRicci[]:=
	Module[{zeroPerms,nonZeroPerms}, 
		If[$RicciEvaluated,
		zeroPerms=Map[If[reservedRicci[[#[[1]],#[[2]]]]==0,#]&,RicciPerms];
		nonZeroPerms=Complement[RicciPerms,zeroPerms];
		Print[">> Printing non-zero components of the Ricci tensor:"];
		If[nonZeroPerms=={}, Print["All components of the Ricci tensor are zero."]];
		Print[Column[Row[{Subscript["R",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]]," = ",reservedRicci[[#[[1]],#[[2]]]]}]&/@nonZeroPerms]],
		ComputeRicci[];
		ShowAllRicci[]]]


Protect[ComputeRicci];


(* ::Section::Closed:: *)
(*Calculate and Display Ricci Scalar*)


ComputeRicciScalar[]:= 
Module[{RicciScalarTiming},
	If[$RicciScalarEvaluated,
		Print["You have already calculated the Ricci scalar."],
		(If[$RicciEvaluated,
			Print["Computing the Ricci scalar."];
			RicciScalarTiming=AbsoluteTiming[reservedRicciScalar = Parallelize[Sum[reservedInverseMetric[[i,j]]reservedRicci[[i,j]],{i,1,reservedDim},{j,1,reservedDim}]]//OurSimplify];$RicciScalarEvaluated=True;
			Print[">> Computed the Ricci scalar in "<>ToString[RicciScalarTiming[[1]]]<>" seconds."];,
			ComputeRicci[];
			ComputeRicciScalar[];]
		Unprotect[Global`RicciScalar];
		Global`RicciScalar=reservedRicciScalar;
		Protect[Global`RicciScalar];
		DistributeDefinitions[reservedRicciScalar];)]]


Protect[ComputeRicciScalar];


(* ::Section::Closed:: *)
(*Calculate Einstein Tensor*)


ComputeEinstein[]:= 
Module[{EinsteinTiming},
	If[$EinsteinEvaluated,
		Print["You have already calculated the Einstein tensor."],
		(If[$RicciScalarEvaluated,
			Print["Computing components of the Einstein tensor."];
				EinsteinTiming=AbsoluteTiming[
					reservedEinstein=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]]}-> reservedRicci[[#[[1]],#[[2]]]]//OurSimplify)&,RicciPerms]],{reservedDim,reservedDim},RicciSyms];];$EinsteinEvaluated=True;
				Print[">> Computed "<>ToString[Length[RicciPerms]]<>" independent components of the Einstein tensor in "<>ToString[EinsteinTiming[[1]]]<>" seconds."],
			ComputeRicciScalar[];
			ComputeEinstein[];];
		Unprotect[Global`Einstein];
		Global`Einstein=reservedEinstein;
		Protect[Global`Einstein];
		DistributeDefinitions[reservedEinstein];)]];



Protect[ComputeRicci];


(* ::Section::Closed:: *)
(*(1,-1) Ricci Tensor*)


ComputeRicciU[]:= 
Module[{RicciUTiming},
	If[$RicciUEvaluated,
		Print["You have already calculated the (+1, -1) Ricci Tensor."],
		(If[$RicciEvaluated,
			Print["Computing components of the (1,1) Ricci tensor."]; 
			RicciUTiming=AbsoluteTiming[
			reservedRicciU=Table[Sum[reservedInverseMetric[[Dummy1,Dummy3]]reservedRicci[[Dummy3,Dummy2]],{Dummy3,1,reservedDim}]//OurSimplify,{Dummy1,1,reservedDim},{Dummy2,1,reservedDim}]];
			$RicciUEvaluated=True;
			Print[">> Computed "<>ToString[reservedDim^2]<>" components of the (1,1) Ricci tensor in "<>ToString[RicciUTiming[[1]]]<>" seconds."],
			ComputeRicci[];ComputeRicciU[]]
		Unprotect[Global`RicciU];
		Global`RicciU=reservedRicciU;
		Protect[Global`RicciU];
		DistributeDefinitions[reservedRicciU];)]]


ShowRicciU[indexList_]/;
If[Length[indexList]!=2,Message[ShowRicciU::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]],True, Message[ShowRicciU::NotIndices];False]]:=
	If[$RicciUEvaluated,
		Print[ Row[{Subscript[Superscript["R",Style[ToString[reservedCoords[[indexList[[1]]]]],Red]],Style[ToString[reservedCoords[[indexList[[2]]]]],Red]]," = ",reservedRicciU[[indexList[[1]],indexList[[2]]]]}]],
		ComputeRicciU[];
		ShowRicciU[indexList]];

ShowRicciU::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowRicciU::IndicesLength="Index list must be length two.";


ShowCoordRicciU[coordList_]/;
If[Length[coordList]!=2,Message[ShowCoordRicciU::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]],True,Message[ShowCoordRicciU::NotCoordinates];False]]:= 
	If[$RicciUEvaluated,
		Print[ Row[{Subscript[Superscript["R",Style[ToString[coordList[[1]]],Red]],Style[ToString[coordList[[2]]],Red]]," = ",reservedRicciU[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]]]]}]],
		ComputeRicciU[];
		ShowCoordRicciU[coordList]];

ShowCoordRicciU::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordRicciU::CoordsLength="Coordinate list must be length two.";


ShowAllRicciU[]:=
	Module[{zeroPerms,nonZeroPerms}, 
		If[$RicciUEvaluated,
			zeroPerms=Map[If[reservedRicci[[#[[1]],#[[2]]]]==0,#]&,RicciPerms];
			nonZeroPerms=Complement[RicciPerms,zeroPerms];
			Print[">> Printing non-zero components of the (+1,-1) Ricci tensor:"];
			If[nonZeroPerms=={}, Print["All components of the (+1,-1) Ricci tensor are zero."]];
			Print[Column[Row[{Subscript[Superscript["R",Style[ToString[reservedCoords[[#[[1]]]]],Red]],Style[ToString[reservedCoords[[#[[2]]]]],Red]]," = ",reservedRicciU[[#[[1]],#[[2]]]]}]&/@nonZeroPerms]],
			ComputeRicciU[];
			ShowAllRicciU[]]]


Protect[ComputeRicciU];


(* ::Section::Closed:: *)
(*Ricci Squared*)


ComputeRicciSquared[]:=
Module[{RicciSquaredTiming},
	If[$RicciSquaredEvaluated,
		Print["You have already calculated Ricci squared."], 
		(If[$RicciUEvaluated,
			Print["Computing the square of the Ricci tensor."]; 
			RicciSquaredTiming=AbsoluteTiming[reservedRicciSquared=Sum[reservedRicciU[[Dummy1,Dummy2]]reservedRicciU[[Dummy2,Dummy1]],{Dummy1,1,reservedDim},{Dummy2,1,reservedDim}]//OurSimplify];
			$RicciSquaredEvaluated=True;
			Print[">> Computed the square of the Ricci tensor in "<>ToString[RicciSquaredTiming[[1]]]<>" seconds."],
			ComputeRicciU[];
			ComputeRicciSquared[]];
		Unprotect[Global`RicciSquared];
		Global`RicciSquared=reservedRicciSquared;
		Protect[Global`RicciSquared];)]]


Protect[ComputeRicciSquared];


(* ::Section::Closed:: *)
(*Calculate and Display Einstein Tensor*)


ComputeEinstein[]:= 
Module[{EinsteinTiming},
	If[$EinsteinEvaluated,
		Print["You have already calculated the Einstein tensor."],
		(If[$RicciScalarEvaluated,
			Print["Computing components of the Einstein tensor."];
			EinsteinTiming=AbsoluteTiming[
			reservedEinstein=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]]}-> (reservedRicci[[#[[1]],#[[2]]]]-reservedRicciScalar/(2) reservedMetric[[#[[1]],#[[2]]]])//OurSimplify)&,RicciPerms]],{reservedDim,reservedDim},RicciSyms];];$EinsteinEvaluated=True;
			Print[">> Computed "<>ToString[Length[RicciPerms]]<>" independent components of the Einstein tensor in "<>ToString[EinsteinTiming[[1]]]<>" seconds."],
			ComputeRicciScalar[];
			ComputeEinstein[];]
		Unprotect[Global`Einstein];
		Global`Einstein=reservedEinstein;
		Protect[reservedEinstein];
		DistributeDefinitions[reservedEinstein];)]]


ShowEinstein[indexList_]/;
If[Length[indexList]!=2,Message[ShowEinstein::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]],True,Message[ShowEinstein::NotIndices];False]]:=
	If[$EinsteinEvaluated,
		Print[ Row[{Subscript["G",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]]," = ",reservedEinstein[[indexList[[1]],indexList[[2]]]]}]],
		ComputeEinstein[];
		ShowEinstein[indexList]];

ShowEinstein::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowEinstein::IndicesLength="Index list must be length two.";


ShowCoordEinstein[coordList_]/;
If[Length[coordList]!=2,Message[ShowCoordEinstein::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]],True,Message[ShowCoordEinstein::NotCoordinates];False]]:= 
	If[$EinsteinEvaluated,
		Print[ Row[{Subscript["G",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]]," = ",reservedEinstein[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]]]]}]],
		ComputeEinstein[];
		ShowCoordEinstein[coordList]];

ShowCoordEinstein::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordEinstein::CoordsLength="Coordinate list must be length two.";


ShowAllEinstein[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$EinsteinEvaluated,
			zeroPerms=Map[If[reservedEinstein[[#[[1]],#[[2]]]]==0,#]&,RicciPerms];
			nonZeroPerms=Complement[RicciPerms,zeroPerms];
			Print[">> Printing non-zero components of the Einstein tensor:"];
			If[nonZeroPerms=={}, Print["All components of the Einstein tensor are zero."]];
			Print[Column[Row[{Subscript["G",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]]," = ",reservedEinstein[[#[[1]],#[[2]]]]}]&/@nonZeroPerms]],
			ComputeEinstein[];
			ShowAllEinstein[]]];


Protect[ComputeEinstein];


(* ::Section::Closed:: *)
(*Calculate and Display Weyl Tensor*)


ComputeWeyl[]:=
Module[{WeylTiming},
	If[$WeylEvaluated,
		Print["You have already calculated the Weyl tensor."], 
		(If[$RicciScalarEvaluated,
			Print["Computing components of the Weyl tensor."]; 
			WeylTiming=AbsoluteTiming[
			reservedWeyl=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]],#[[4]]}-> reservedRiemann[[#[[1]],#[[2]],#[[3]],#[[4]]]]+1/(reservedDim-2) (reservedMetric[[#[[1]],#[[4]]]]*reservedRicci[[#[[2]],#[[3]]]]-reservedMetric[[#[[1]],#[[3]]]]*reservedRicci[[#[[2]],#[[4]]]]+reservedMetric[[#[[2]],#[[3]]]]*reservedRicci[[#[[1]],#[[4]]]]-reservedMetric[[#[[2]],#[[4]]]]*reservedRicci[[#[[1]],#[[3]]]])+reservedRicciScalar/((reservedDim-1)*(reservedDim-2)) (reservedMetric[[#[[2]],#[[4]]]]reservedMetric[[#[[1]],#[[3]]]]-reservedMetric[[#[[2]],#[[3]]]]reservedMetric[[#[[1]],#[[4]]]])//OurSimplify)&,RiemannPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannSyms];];
			$WeylEvaluated=True;
			Print[">> Computed "<>ToString[Length[RiemannPerms]]<>" independent components of the Weyl tensor in "<>ToString[WeylTiming[[1]]]<>" seconds."],
			ComputeRicciScalar[];ComputeWeyl[]];
			Unprotect[Global`Weyl];
			Global`Weyl=reservedWeyl;
			Protect[Global`Weyl];
			DistributeDefinitions[reservedWeyl];)]]


ShowWeyl[indexList_]/;
If[Length[indexList]!= 4,Message[ShowWeyl::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[4]]<= Length[reservedCoords]],True,Message[ShowWeyl::NotIndices];False]]:=
	If[$WeylEvaluated,
		Print[ Row[{Subscript["C",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]]<>ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedWeyl[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeWeyl[];
		ShowWeyl[indexList]];

ShowWeyl::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowWeyl::IndicesLength="Index list must be length four.";


ShowCoordWeyl[coordList_]/;
If[Length[coordList]!= 4,Message[ShowCoordWeyl::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordWeyl::NotCoordinates];False]]:= 
	If[$WeylEvaluated,
		Print[ Row[{Subscript["C",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]]<>ToString[coordList[[3]]]<>ToString[coordList[[4]]],Red]]," = ",reservedWeyl[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeWeyl[];
		ShowCoordWeyl[coordList]];

ShowCoordWeyl::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordWeyl::CoordsLength="Coordinate list must be length four.";


ShowAllWeyl[]:=
	Module[{zeroPerms,nonZeroPerms},
		If[$WeylEvaluated,
			zeroPerms=Map[If[reservedWeyl[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannPerms];
			nonZeroPerms=Complement[RiemannPerms,zeroPerms];
			Print[">> Printing non-zero components of the Weyl tensor:"];
			If[nonZeroPerms=={}, Print["All components of the Weyl tensor are zero."]];
			Print[Column[Row[{Subscript["C",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]]<>ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedWeyl[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeWeyl[];
			ShowAllWeyl[]]]


Protect[ComputeWeyl];


(* ::Section::Closed:: *)
(*(+2,-2) Weyl Tensor*)


ComputeWeylUU[]:=
Module[{WeylUUTiming},
	If[$WeylUUEvaluated,
		Print["You have already calculated the (+2,-2) Weyl tensor."], 
		(If[$WeylEvaluated,Print["Computing components of the (+2,-2) Weyl tensor."];
			WeylUUTiming=AbsoluteTiming[
			reservedWeylUU=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]],#[[4]]}->Sum[(reservedInverseMetric[[#[[1]],Dummy1]] reservedInverseMetric[[#[[2]],Dummy2]]-reservedInverseMetric[[#[[1]],Dummy2]] reservedInverseMetric[[#[[2]],Dummy1]]) reservedWeyl[[Dummy1,Dummy2,#[[3]],#[[4]]]],{Dummy1,1,reservedDim},{Dummy2,1,Dummy1}]//OurSimplify)&,RiemannUUPerms]],{reservedDim,reservedDim,reservedDim,reservedDim},RiemannUUSyms];];
			$WeylUUEvaluated=True;
			Print[">> Computed "<>ToString[Length[RiemannUUPerms]]<>" independent components of the (+2,-2) Weyl tensor in "<>ToString[WeylUUTiming[[1]]]<>" seconds."],
			ComputeWeyl[];
			ComputeWeylUU[];]
		Unprotect[Global`WeylUU];
		Global`WeylUU=reservedWeylUU;
		Protect[Global`WeylUU];
		DistributeDefinitions[reservedWeylUU];)]]


ShowWeylUU[indexList_]/;
If[Length[indexList]!=4,Message[ShowWeylUU::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[4]]<= Length[reservedCoords]],True,Message[ShowWeylUU::NotIndices];False]]:=
	If[$WeylUUEvaluated,
		Print[ Row[{Subscript[Superscript["C",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]],Style[ToString[reservedCoords[[indexList[[3]]]]]<>ToString[reservedCoords[[indexList[[4]]]]],Red]]," = ",reservedWeylUU[[indexList[[1]],indexList[[2]],indexList[[3]],indexList[[4]]]]}]],
		ComputeWeylUU[];
		ShowWeylUU[indexList]];

ShowWeylUU::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowWeylUU::IndicesLength="Index list must be length four.";


ShowCoordWeylUU[coordList_]/;
If[Length[coordList]!=4,Message[ShowCoordWeylUU::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]]&&MemberQ[reservedCoords,coordList[[4]]],True,Message[ShowCoordWeylUU::NotCoordinates];False]]:= 
	If[$WeylUUEvaluated,
		Print[ Row[{Subscript[Superscript["C",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]],Style[ToString[coordList[[3]]]<>ToString[coordList[[4]]],Red]]," = ",reservedWeylUU[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]],Position[reservedCoords,coordList[[4]]][[1,1]]]]}]],
		ComputeWeylUU[];
		ShowCoordWeylUU[coordList]];

ShowCoordWeylUU::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordWeylUU::CoordsLength="Coordinate list must be length four.";


ShowAllWeylUU[]:= 
	Module[{zeroPerms,nonZeroPerms},
		If[$WeylUUEvaluated,
			zeroPerms=Map[If[reservedWeylUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]==0,#]&,RiemannUUPerms];
			nonZeroPerms=Complement[RiemannUUPerms,zeroPerms];
			Print[">> Printing non-zero components of the (+2, -2) Weyl tensor:"];
			If[nonZeroPerms=={}, Print["All components of the (+2, -2) Weyl tensor are zero."]];
			Print[Column[Row[{Subscript[Superscript["C",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]],Style[ToString[reservedCoords[[#[[3]]]]]<>ToString[reservedCoords[[#[[4]]]]],Red]]," = ",reservedWeylUU[[#[[1]],#[[2]],#[[3]],#[[4]]]]}]&/@nonZeroPerms]],
			ComputeWeylUU[];
			ShowAllWeylUU[]]]


Protect[ComputeWeylUU];


(* ::Section::Closed:: *)
(*Weyl Squared*)


ComputeWeylSquared[]:= 
Module[{WeylSquaredTiming},
	If[$WeylSquaredEvaluated,
		Print["You have already calculated Weyl squared."], 
		(If[$WeylUUEvaluated,
			Print["Computing the square of the Weyl tensor."]; 
			WeylSquaredTiming=AbsoluteTiming[reservedWeylSquared=Total[reservedWeylUU[[RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]],RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]]]]reservedWeylUU[[RiemannUUPerms[[#,3]],RiemannUUPerms[[#,4]],RiemannUUPerms[[#,1]],RiemannUUPerms[[#,2]]]]*RiemannUUDepCompLengths[[#]]&/@Range[Length[RiemannUUPerms]]]//OurSimplify];
			$WeylSquaredEvaluated=True;
			Print[">> Computed the square of the Weyl tensor in "<>ToString[WeylSquaredTiming[[1]]]<>" seconds."],
			ComputeWeylUU[];
			ComputeWeylSquared[]];
		Unprotect[Global`WeylSquared];
		Global`WeylSquared=reservedWeylSquared;
		Protect[Global`WeylSquared];
		DistributeDefinitions[reservedWeylSquared];)]]


Protect[ComputeWeylSquared];


(* ::Section::Closed:: *)
(*Calculate and Display Cotton Tensor*)


ComputeCotton[]:= 
Module[{CottonTiming},
	If[$CottonEvaluated,
		Print["You have already calculated the Cotton tensor."],
		(If[$RicciScalarEvaluated,
			Print["Computing components of the Cotton tensor."];
				CottonTiming=AbsoluteTiming[
					reservedCotton=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]],#[[3]]}-> D[reservedRicci[[#[[1]],#[[2]]]],reservedCoords[[#[[3]]]]]-Sum[reservedChristoffel[[\[Lambda],#[[3]],#[[1]]]]reservedRicci[[\[Lambda],#[[2]]]]+reservedChristoffel[[\[Lambda],#[[3]],#[[2]]]]reservedRicci[[#[[1]],\[Lambda]]],{\[Lambda],1,reservedDim}]-(D[reservedRicci[[#[[1]],#[[3]]]],reservedCoords[[#[[2]]]]]-Sum[reservedChristoffel[[\[Lambda],#[[2]],#[[1]]]]reservedRicci[[\[Lambda],#[[3]]]]+reservedChristoffel[[\[Lambda],#[[2]],#[[3]]]]reservedRicci[[#[[1]],\[Lambda]]],{\[Lambda],1,reservedDim}])+ 1/(2(reservedDim-1)) (reservedMetric[[#[[1]],#[[3]]]]D[reservedRicciScalar,reservedCoords[[#[[2]]]]]-reservedMetric[[#[[1]],#[[2]]]]D[reservedRicciScalar,reservedCoords[[#[[3]]]]])//OurSimplify)&,CottonPerms]],{reservedDim,reservedDim,reservedDim},CottonSyms];];
					$CottonEvaluated=True;
				Print[">> Computed "<>ToString[Length[CottonPerms]]<>" independent components of the Cotton tensor in "<>ToString[CottonTiming[[1]]]<>" seconds."],
			ComputeRicciScalar[];
			ComputeCotton[];];
		Unprotect[Global`Cotton];
		Global`Cotton=reservedCotton;
		Protect[Global`Cotton];
		DistributeDefinitions[reservedCotton];)]];



ShowCotton[indexList_]/;
If[Length[indexList]!=3,Message[ShowCotton::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[3]]<= Length[reservedCoords]],True,Message[ShowCotton::NotIndices];False]]:=
	If[$CottonEvaluated,
		Print[ Row[{Subscript["C",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]]<>ToString[reservedCoords[[indexList[[3]]]]],Red]]," = ",reservedCotton[[indexList[[1]],indexList[[2]],indexList[[3]]]]}]],
		ComputeCotton[];
		ShowCotton[indexList]];

ShowCotton::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowCotton::IndicesLength="Index list must be length three.";


ShowCoordCotton[coordList_]/;
If[Length[coordList]!=3,Message[ShowCoordCotton::CoordsLength];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]]&&MemberQ[reservedCoords,coordList[[3]]],True,Message[ShowCoordCotton::NotCoordinates];False]]:= 
	If[$CottonEvaluated,
		Print[ Row[{Subscript["C",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]]<>ToString[coordList[[3]]],Red]]," = ",reservedCotton[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]],Position[reservedCoords,coordList[[3]]][[1,1]]]]}]],
		ComputeCotton[];
		ShowCoordCotton[coordList]];

ShowCoordCotton::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordCotton::CoordsLength="Coordinate list must be length four.";


ShowAllCotton[]:=
	Module[{zeroPerms,nonZeroPerms},
		If[$CottonEvaluated,
			zeroPerms=Map[If[reservedCotton[[#[[1]],#[[2]],#[[3]]]]==0,#]&,CottonPerms];
			nonZeroPerms=Complement[CottonPerms,zeroPerms];
			Print[">> Printing non-zero components of the Cotton tensor:"]; 
			If[nonZeroPerms=={}, Print["All components of the Cotton tensor are zero."]];
			Print[Column[Row[{Subscript["C",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]]<>ToString[reservedCoords[[#[[3]]]]],Red]]," = ",reservedCotton[[#[[1]],#[[2]],#[[3]]]]}]&/@nonZeroPerms]],
			ComputeCotton[];
			ShowAllCotton[]]]


Protect[ComputeCotton];


(* ::Section::Closed:: *)
(*Calculate and Display Schouten Tensor*)


ComputeSchouten[]:= 
Module[{SchoutenTiming},
	If[$SchoutenEvaluated,
		Print["You have already calculated the Schouten tensor."],
		(If[$RicciScalarEvaluated,
			Print["Computing components of the Schouten tensor."];
				SchoutenTiming=AbsoluteTiming[
					reservedSchouten=SymmetrizedArray[Parallelize[Map[({#[[1]],#[[2]]}-> 1/(reservedDim-2) (reservedRicci[[#[[1]],#[[2]]]]-reservedRicciScalar/(2 (reservedDim -1)) reservedMetric[[#[[1]],#[[2]]]])//OurSimplify)&,RicciPerms]],{reservedDim,reservedDim},RicciSyms];];$SchoutenEvaluated=True;
				Print[">> Computed "<>ToString[Length[RicciPerms]]<>" independent components of the Schouten tensor in "<>ToString[SchoutenTiming[[1]]]<>" seconds."],
			ComputeRicciScalar[];
			ComputeSchouten[];];
		Unprotect[Global`Schouten];
		Global`Schouten=reservedSchouten;
		Protect[Global`Schouten];
		DistributeDefinitions[reservedSchouten];)]];



ShowSchouten[indexList_]/;
If[Length[indexList]!=2,Message[ShowSchouten::IndicesLength];False,If[TrueQ[1<= indexList[[1]]<= Length[reservedCoords]]&&TrueQ[1<= indexList[[2]]<= Length[reservedCoords]],True,Message[ShowSchouten::NotIndices];False]]:=
	If[$SchoutenEvaluated,
		Print[ Row[{Subscript["S",Style[ToString[reservedCoords[[indexList[[1]]]]]<>ToString[reservedCoords[[indexList[[2]]]]],Red]]," = ",reservedSchouten[[indexList[[1]],indexList[[2]]]]}]],
		ComputeSchouten[];
		ShowSchouten[indexList]];

ShowSchouten::NotIndices="The arguments given to this function must be numbers indicating the positions of each coordinate in the list of coordinates.";
ShowSchouten::IndicesLength="Index list must be length two.";


ShowCoordSchouten[coordList_]/;
If[Length[coordList]!=2,Message[ShowCoordSchouten::CoordsList];False,If[MemberQ[reservedCoords,coordList[[1]]]&&MemberQ[reservedCoords,coordList[[2]]],True,Message[ShowCoordSchouten::NotCoordinates];False]]:= 
	If[$SchoutenEvaluated,
		Print[ Row[{Subscript["S",Style[ToString[coordList[[1]]]<>ToString[coordList[[2]]],Red]]," = ",reservedSchouten[[Position[reservedCoords,coordList[[1]]][[1,1]],Position[reservedCoords,coordList[[2]]][[1,1]]]]}]],
		ComputeSchouten[];
		ShowCoordSchouten[coordList]];

ShowCoordSchouten::NotCoordinates="The arguments given to this function must be the names of coordinates.";
ShowCoordSchouten::CoordsLength="Coordinate list must be length four.";


ShowAllSchouten[]:=
	Module[{zeroPerms,nonZeroPerms}, 
		If[$SchoutenEvaluated,
		zeroPerms=Map[If[reservedSchouten[[#[[1]],#[[2]]]]==0,#]&,RicciPerms];
		nonZeroPerms=Complement[RicciPerms,zeroPerms];
		Print[">> Printing non-zero components of the Schouten tensor:"];
		If[nonZeroPerms=={}, Print["All components of the Schouten tensor are zero."]];
		Print[Column[Row[{Subscript["S",Style[ToString[reservedCoords[[#[[1]]]]]<>ToString[reservedCoords[[#[[2]]]]],Red]]," = ",reservedSchouten[[#[[1]],#[[2]]]]}]&/@nonZeroPerms]],
		ComputeSchouten[];
		ShowAllSchouten[]]]


Protect[ComputeSchouten];


(* ::Section::Closed:: *)
(*ShowArray*)


arrayList={"Christoffel","Riemann","RiemannUU","DualRiemann","DualRiemannUU","Ricci","RicciU","Einstein","Weyl","WeylUU","Cotton","Schouten"};


ShowArray[arrayName_String,coords_List:{False}]:=
	If[!MemberQ[arrayList,arrayName],
		Print[arrayName<>" is not an array currently implemented in GRUMPS. For the list of currently implemented arrays, see ",Style["GRUMPSCommands[]",Blue]],
		If[coords=={False},ToExpression["Private`ShowAll"<>arrayName<>"[]"];,If[IntegerQ[coords[[1]]],ToExpression["Private`Show"<>arrayName<>"["<>ToString[coords]<>"]"];,ToExpression["Private`ShowCoord"<>arrayName<>"["<>ToString[coords]<>"]"];]]];


(* ::Section:: *)
(**)


End[]
EndPackage[]
