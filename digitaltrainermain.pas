UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Menus,baseGate,logicGates;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    ButtonAddCustom: TButton;
    ButtonAddNxor: TButton;
    ButtonAddNor: TButton;
    ButtonAddNand: TButton;
    ButtonAddXor: TButton;
    ButtonAddNot: TButton;
    ButtonAddOr: TButton;
    ButtonAddAnd: TButton;
    ButtonAddOutput: TButton;
    ButtonAddInput: TButton;
    DeleteButton: TButton;
    captionEdit: TEdit;
    FlowPanel1: TFlowPanel;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    descriptionMemo: TMemo;
    GroupBox5: TGroupBox;
    CustomGateListBox: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    SimTimer: TTimer;
    Splitter2: TSplitter;
    wireImage: TImage;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    ZoomTrackBar: TTrackBar;
    PROCEDURE ButtonAddCustomClick(Sender: TObject);
    PROCEDURE ButtonAddAndClick(Sender: TObject);
    PROCEDURE ButtonAddInputClick(Sender: TObject);
    PROCEDURE ButtonAddNandClick(Sender: TObject);
    PROCEDURE ButtonAddNorClick(Sender: TObject);
    PROCEDURE ButtonAddNotClick(Sender: TObject);
    PROCEDURE ButtonAddNxorClick(Sender: TObject);
    PROCEDURE ButtonAddOrClick(Sender: TObject);
    PROCEDURE ButtonAddOutputClick(Sender: TObject);
    PROCEDURE ButtonAddXorClick(Sender: TObject);
    PROCEDURE DeleteButtonClick(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE SimTimerTimer(Sender: TObject);
    PROCEDURE ZoomTrackBarChange(Sender: TObject);
  private
    workspace:T_workspace;
  public
  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TDigitaltrainerMainForm }

FUNCTION workspaceFilename:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.workspace');
  end;

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  begin
    workspace.create;
    workspace.loadFromFile(workspaceFilename);
    workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,wireImage);
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.saveToFile(workspaceFilename);
    workspace.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.DeleteButtonClick(Sender: TObject);
  begin
    workspace.currentBoard^.deleteMarkedGate;
  end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddInputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_input,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNandClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nandGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_orGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNotClick(Sender: TObject);
  begin workspace.addBaseGate(gt_notGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNxorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nxorGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOrClick(Sender: TObject);
  begin workspace.addBaseGate(gt_orGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddAndClick(Sender: TObject);
  begin workspace.addBaseGate(gt_andGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddCustomClick(Sender: TObject);
  begin workspace.addCustomGate(CustomGateListBox.ItemIndex,0,0); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOutputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_output,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddXorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_xorGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.currentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.SimTimerTimer(Sender: TObject);
  begin
    workspace.currentBoard^.simulateStep;
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomTrackBarChange(Sender: TObject);
  begin
    workspace.currentBoard^.setZoom(ZoomTrackBar.position);
  end;

end.

