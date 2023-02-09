UNIT createTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

TYPE

  { TCreateTaskForm }

  TCreateTaskForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    memo1: TMemo;
    rbIncludeAllGates: TRadioButton;
    rbIncludeIO: TRadioButton;
    rbIncludeHalfOfGates: TRadioButton;
    rbIncludeNothing: TRadioButton;
    TrackBar1: TTrackBar;
  private

  public

  end;

VAR
  CreateTaskForm: TCreateTaskForm;

IMPLEMENTATION

{$R *.lfm}

end.

