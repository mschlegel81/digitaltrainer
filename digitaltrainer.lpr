PROGRAM digitaltrainer;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, digitaltrainerMain, propertyDialog, analysisDialog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.title:='Digitaltrainer';
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TDigitaltrainerMainForm, DigitaltrainerMainForm);
  Application.CreateForm(TgatePropertyDialog, gatePropertyDialog);
  Application.CreateForm(TanalysisForm, analysisForm);
  Application.run;
end.

