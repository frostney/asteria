unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, SynEdit, SynHighlighterJScript,
  SynCompletion, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons,
  BESEN, BESENConstants, BESENObjectGlobal, BESENErrors, BESENStringUtils, BESENObject, BESENValue, BESENObjectPropertyDescriptor, BESENObjectConsole,
  ECMAScriptHarmonyShim,
  AsteriaGlobal, AsteriaEditorHelper;

type
  TStatusType = (stNone, stError);

  { TfrmMain }

  TfrmMain = class(TForm)
    btnNew: TBitBtn;
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnDeploy: TBitBtn;
    btnRun: TBitBtn;
    btnQuit: TBitBtn;
    btnClear: TButton;
    chbLiveEdit: TCheckBox;
    FontDialog: TFontDialog;
    memOutput: TMemo;
    OpenDialog: TOpenDialog;
    OpenGLControl: TOpenGLControl;
    pnlStatus: TPanel;
    pnlOutput: TPanel;
    pnlPreview: TPanel;
    pnlEditor: TPanel;
    pnlMain: TPanel;
    pnlToolbar: TPanel;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    SynCompletion: TSynCompletion;
    SynEdit: TSynEdit;
    SynJScriptSyn: TSynJScriptSyn;
    timLoop: TTimer;
    ToggleBox1: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    procedure btnRunClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure timLoopTimer(Sender: TObject);


  private
    { private declarations }
  public
    procedure EvaluateCode(const Command: String);
    procedure ShowStatusMessage(const Message: String; const StatusType: TStatusType = stNone);
  end;

var
  frmMain: TfrmMain;
  EditorHelper: TasEditorHelper;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ObjWindow: TBESENObject;
begin
  // Create BESEN instance
  BesenInst := TBesen.Create(COMPAT_JS); //< We want JavaScript compability at all costs

  BesenInst.RecursionLimit := 128;

  EditorHelper := TasEditorHelper.Create;

  TBESEN(BesenInst).ObjectGlobal.RegisterNativeFunction('println', @EditorHelper.PrintLn, 1, []);

  ObjWindow:=TBESEN(BesenInst).ObjectGlobal;
  TBESEN(BesenInst).ObjectGlobal.OverwriteData('window', BESENObjectValue(ObjWindow), [bopaWRITABLE,bopaCONFIGURABLE]);

  TBESEN(BesenInst).InjectObject('console', BESENConvertToUTF8(BESENObjectConsoleSource));
  TBESEN(BesenInst).Execute(BESENConvertToUTF8(ECMAScriptHarmonyShimSource));
end;

procedure TfrmMain.FormDeactivate(Sender: TObject);
begin
  BesenInst.Free;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  memOutput.Lines.Clear;
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
  Self.EvaluateCode(SynEdit.Text);
end;

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmMain.SynEditChange(Sender: TObject);
begin
  if (chbLiveEdit.Checked) then
    Self.EvaluateCode(SynEdit.Text);
end;

procedure TfrmMain.timLoopTimer(Sender: TObject);
begin
  OpenGLControl.SwapBuffers;
end;

procedure TfrmMain.EvaluateCode(const Command: String);
begin
  ShowStatusMessage('');

  try
    BesenInst.Execute(BESENConvertToUTF8(Command));
  except
    on e: EBESENError do
        ShowStatusMessage(Format('%s ( %s | Line %d ): %s', [e.Name, '', TBESEN(BesenInst).LineNumber, e.Message]), stError);

    on e: exception do
        ShowStatusMessage(Format('%s ( %s | Line %d ): %s', ['Exception', '', TBESEN(BesenInst).LineNumber, e.Message]), stError);
  end;
end;

procedure TfrmMain.ShowStatusMessage(const Message: String;
  const StatusType: TStatusType);
begin
  case StatusType of
    stNone: pnlStatus.Color := $00777777;
    stError: pnlStatus.Color := clRed;
  end;

  pnlStatus.Caption := Message;
end;

end.

