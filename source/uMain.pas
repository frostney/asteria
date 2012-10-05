unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, SynEdit, SynHighlighterJScript,
  SynCompletion, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, FileCtrl,
  BESEN, BESENConstants, BESENObjectGlobal, BESENErrors, BESENStringUtils, BESENObject, BESENValue, BESENObjectPropertyDescriptor, BESENObjectConsole,
  ECMAScriptHarmonyShim,
  AsteriaGlobal, AsteriaEditorHelper, AsteriaFileSystem, AsteriaDocument;

type
  TStatusType = (stNone, stError);

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClear: TButton;
    btnNew: TBitBtn;
    btnOpen: TBitBtn;
    btnOpen1: TBitBtn;
    btnInfo: TBitBtn;
    btnSave: TBitBtn;
    btnDeploy: TBitBtn;
    btnRun: TBitBtn;
    btnQuit: TBitBtn;
    btnSave1: TBitBtn;
    btnSave2: TBitBtn;
    FileListBox1: TFileListBox;
    FontDialog: TFontDialog;
    memOutput: TMemo;
    OpenDialog: TOpenDialog;
    OpenGLControl: TOpenGLControl;
    pnlOutputButtonBar: TPanel;
    pnlStatus: TPanel;
    pnlOutput: TPanel;
    pnlPreview: TPanel;
    pnlEditor: TPanel;
    pnlMain: TPanel;
    pnlToolbar: TPanel;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    Splitter1: TSplitter;
    SynCompletion: TSynCompletion;
    SynEdit: TSynEdit;
    SynJScriptSynLight: TSynJScriptSyn;
    SynJScriptSynDark: TSynJScriptSyn;
    timLoop: TTimer;
    btnLiveEdit: TToggleBox;
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
  FileSystem: TasFileSystem;
  PathModule: TasPathModule;
  Document: TasDocument;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ObjWindow: TBESENObject;
  ObjFileSystem: TBESENObject;
  ObjDocument: TBESENObject;
  ObjPath: TBESENObject;
begin
  // Create BESEN instance
  BesenInst := TBesen.Create(COMPAT_JS); //< We want JavaScript compability at all costs

  BesenInst.RecursionLimit := 128;

  EditorHelper := TasEditorHelper.Create;
  FileSystem := TasFileSystem.Create;
  Document := TasDocument.Create;

  // Println is needed
  TBESEN(BesenInst).ObjectGlobal.RegisterNativeFunction('println', @EditorHelper.PrintLn, 1, []);
  TBESEN(BesenInst).ObjectGlobal.RegisterNativeFunction('require', @EditorHelper.Require, 1, []);

  ObjWindow:=TBESEN(BesenInst).ObjectGlobal;
  TBESEN(BesenInst).ObjectGlobal.OverwriteData('window', BESENObjectValue(ObjWindow), [bopaWRITABLE,bopaCONFIGURABLE]);
  ObjWindow.OverwriteData('asteria', BESENBooleanValue(true), [bopaWRITABLE,bopaCONFIGURABLE]);
  ObjWindow.OverwriteData('innerWidth', BESENNumberValue(OpenGLControl.Width), []);
  ObjWindow.OverwriteData('innerHeight', BESENNumberValue(OpenGLControl.Height), []);

  ObjFileSystem := TBESENObject.Create(BesenInst, TBESEN(BesenInst).ObjectPrototype, false);
  TBESEN(BesenInst).ObjectGlobal.OverwriteData('fs', BESENObjectValue(ObjFileSystem), [bopaWRITABLE,bopaCONFIGURABLE]);
  TBESEN(BesenInst).GarbageCollector.Add(ObjFileSystem);
  ObjFileSystem.RegisterNativeFunction('fileExists', @FileSystem.FileExists, 1, []);
  ObjFileSystem.RegisterNativeFunction('dirExists', @FileSystem.DirExists, 1, []);
  ObjFileSystem.RegisterNativeFunction('exists', @FileSystem.DirExists, 1, []);
  ObjFileSystem.RegisterNativeFunction('readFile', @FileSystem.ReadFile, 1, []);
  ObjFileSystem.RegisterNativeFunction('writeFile', @FileSystem.WriteFile, 2, []);
  ObjFileSystem.RegisterNativeFunction('rename', @FileSystem.Rename, 2, []);

  ObjPath := TBESENObject.Create(BesenInst, TBESEN(BesenInst).ObjectPrototype, false);
  TBESEN(BesenInst).ObjectGlobal.OverwriteData('path', BESENObjectValue(ObjPath), [bopaWRITABLE,bopaCONFIGURABLE]);
  TBESEN(BesenInst).GarbageCollector.Add(ObjPath);
  ObjPath.OverwriteData('sep', BESENStringValue(DirectorySeparator));
  ObjPath.RegisterNativeFunction('join', @PathModule.Join, 100, []);
  ObjPath.RegisterNativeFunction('basename', @PathModule.Basename, 0, []);
  ObjPath.RegisterNativeFunction('extname', @PathModule.Extname, 0, []);
  ObjPath.RegisterNativeFunction('relative', @PathModule.Relative, 0, []);


  ObjDocument := TBESENObject.Create(BesenInst, TBESEN(BesenInst).ObjectPrototype, false);
  TBESEN(BesenInst).ObjectGlobal.OverwriteData('document', BESENObjectValue(ObjDocument), [bopaWRITABLE,bopaCONFIGURABLE]);
  ObjDocument.RegisterNativeFunction('getElementById', @Document.GetElementById, 1, []);
  ObjDocument.RegisterNativeFunction('addEventListener', @Document.AddEventListener, 3, []);


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
  if (btnLiveEdit.Checked) then
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

    on e: Exception do
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

