{ ==============================================================================

  MAIN FORM

    This form is the main management form for Testmaster. A list of any
    available test files (TMQ) is shown in the left box. The associated
    summary information is shown at the right. Buttons are provided to start
    the selected test, or to Add/Edit/Delete a test (question set).


  LICENSE:

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

  =============================================================================}
                                                                    
{ TODO : Write convertor to change old files from TMQ to TMQZ }

{ TODO : Compile, test and amend, on Windows X64. Check button orders OK }

{ TODO : Update exhibit manager to look for files anywhere }
{ TODO : If change/delete Exhibit, delete redundant image file on save }

{ TODO : BUG : FontSize/Hints not working in RunTestForm. Works ok in AddEditForm? }
{ TODO : BUG : How keep selected item in view in scrollbar (e.g. Goto listbox ) }     
{ TODO : BUG : Saved results are sometimes wrong, review issue? }

unit uMainForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, FileCtrl,
  FileUtil, Menus, Graphics, Math,
  //
  uQuestionSet, uGetPasswordForm, uJsonCfgFile, uCommon;

type

  { TMainForm }

  TMainForm = class(TForm)
    FileListBox1: TFileListBox;
    lblDropHelp: TLabel;
    memoInfo: TMemo;
    MenuItem1: TMenuItem;
    menuAboutMac: TMenuItem;
    menuMac: TMenuItem;
    N5: TMenuItem;
    menuResults: TMenuItem;
    Panel1: TPanel;
    imgLogo: TImage;
    btnEdit: TButton;
    btnStart: TButton;
    btnExit: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    MainMenu1: TMainMenu;
    menuApplication: TMenuItem;
    menuHelp: TMenuItem;
    menuExitWindows: TMenuItem;
    N3: TMenuItem;
    menuTestmasterHelp: TMenuItem;
    menuSep2: TMenuItem;
    menuAboutWindows: TMenuItem;
    menuSep1: TMenuItem;
    menuAdministrator: TMenuItem;
    menuFontSizeSmall: TMenuItem;
    menuFontSizeMedium: TMenuItem;
    menuFontsizeLarge: TMenuItem;
    menuShowHints: TMenuItem;
    btnHelp: TButton;
    menuElapsedTime: TMenuItem;
    menuTimeRemaining: TMenuItem;
    N1: TMenuItem;
    Panel2: TPanel;
    panelLogo: TPanel;
    procedure btnEditClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure MenuResultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure menuTestmasterHelpClick(Sender: TObject);
    procedure menuAboutWindowsClick(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);
    procedure menuAdministratorClick(Sender: TObject);
    procedure FontSizeClick(Sender: TObject);
    procedure menuShowHintsClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnHelpClick(Sender: TObject);
    procedure ElapsedTimeClick(Sender: TObject);
  private
    QSet: TQuestionSet;
    procedure CreateLockedDownStatus;
    procedure DoAdministrator;
    function LoadLogo(LogoFilename: string =  ''): boolean;
    procedure WriteStr(Stream: TFileStream; Value: string);
    procedure ReadStatus(IsFirst: boolean = False);
    procedure ReadStr(Stream: TFileStream; var Value: string);
    procedure ShowSummary;
  public
    procedure WriteStatus;
  end;

var
  MainForm: TMainForm;


implementation

uses
  uAddEditForm, uAddFile, uAboutForm, uRunTestForm, uResultsForm, uOptionsForm,
  uTimerValueForm;

{$R *.lfm}

const
  SECT_CFG = 'MainForm';
  CFG_FILENAME = 'QFile';

  TESTMASTER_OPT_FILE = 'TestMaster.opt4';
  TESTMASTER_CFG_FILE = 'TestMaster.cfg';
  TESTMASTER_OPT_VER  = 'OPT_V4';
  DEFAULT_ADMIN_PW    = 'FIBONACCI';
  QSET_ZIP_FILENAME   = 'qset.tmq4';
  TESTMASTER_EXT      = '.tmqz';
  DROP_EXTS           = TESTMASTER_EXT;
  FILELISTBOX_MAX_WIDTH = 300;

{ CREATE }

procedure TMainForm.FormCreate(Sender: TObject);
var
  tmpDir: string;
begin                          
  {$ifdef darwin}
    menuMac.Caption := #$EF#$A3#$BF;    // 'Apple logo', add this to App menu
    menuMac.Visible := True;            //  Default for this is not visible
  {$else}                                                                  
    menuSep1.Visible := True;           // Default for these is not visible
    menuExitWindows.Visible := True;
    menuTestmasterHelp.Caption := 'Help &Index';
    menuSep2.Visible := True;
    menuAboutWindows.Visible := True;
  {$endif}

  tmpDir := GetAppDataDirectory(adGlobal);
  if (not DirectoryExists(tmpDir)) then     // Ensure there is an App Data folder
    ForceDirectories(tmpDir);
  AppCfg := TConfigFile.Create(tmpDir + TESTMASTER_CFG_FILE);
  //
  tmpDir := GetAppTestsDirectory;
  if (not DirectoryExists(tmpDir)) then     // Ensure there is a Tests folder
    ForceDirectories(tmpDir);
  //
  AppTempDirectory := GetAppTempDirectory;  // For unzipped TMQZ files
  //
  QSet := TQuestionSet.Create;
  AppPassword := '';
  AppTimer := 60;
  //
  Left := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_LEFT, 40);
  Top := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_TOP, 40);
  Width := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_WIDTH, 0);
  Height := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_HEIGHT, 0);
  AppQFilename := AppCfg.ReadString(SECT_CFG, CFG_FILENAME, '');
end;


{ DESTROY }

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_LEFT, Left);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_TOP, Top);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_WIDTH, Width);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_HEIGHT, Height);
  AppCfg.WriteString(SECT_CFG, CFG_FILENAME, AppQFilename);
  //
  QSet.Free;
  AppCfg.Free;
  DeleteDirectory(AppTempDirectory, False);
end;


{ SHOW }

procedure TMainForm.FormShow(Sender: TObject);
var
  ThisIndex: integer;
begin
  // Get status. If question sets exist then list them
  ReadStatus(True);
  FileListBox1.Directory := GetAppTestsDirectory;
  FileListBox1.UpdateFileList;
  if (FileListBox1.Items.Count = 0) then
    begin
      btnStart.Enabled  := False;
      btnEdit.Enabled   := False;
      btnDelete.Enabled := False;
      LoadLogo;
      memoInfo.Text := 'No question databases found in "' + GetAppTestsDirectory + '"';
      AppQFilename := '';
    end
  else
    begin
      ThisIndex := 0;
      if (AppQFilename <>  '') then     // Previous file viewed?
        begin
          ThisIndex := FileListBox1.Items.IndexOf(AppQFilename);
          if (ThisIndex =  -1) then     // Not found, might have been deleted?
            ThisIndex :=  0;
        end;
      FileListBox1.ItemIndex := ThisIndex;
      btnStart.Enabled  := True;
      btnEdit.Enabled   := True;
      btnDelete.Enabled := AdminOptions.IsDeleteEnabled;
      ShowSummary;
    end;
end;


{ LOAD LOGO }

{ If a filename is provided then load requested logo if it exists. If empty
  filename then check for a custom logo in the global data folder, otherwise
  load the default logo from the AppDirectory }

function TMainForm.LoadLogo(LogoFilename: string): boolean;
var
  ThisLogoFilename: string;
begin
  Result := True;
  ThisLogoFilename := LogoFilename;
  if (ThisLogoFilename = '') then
    begin
      ThisLogoFilename := GetAppDataDirectory(adGlobal) + 'custom_logo.png';
      if (not FileExists(ThisLogoFilename)) then
        ThisLogoFilename := GetAppDirectory + 'logo.png';
    end;
  if (FileExists(ThisLogoFilename)) then
    begin
      LoadImage(imgLogo, ThisLogoFilename);
      panelLogo.Height := Min(120, imgLogo.Picture.Height); // Max logo height = 120
      panelLogo.Caption := '';
    end
  else
    begin
      imgLogo.Picture.Clear;
      panelLogo.Caption := 'Cannot find logo file:' + CRLF + ThisLogoFilename;
      Result := False;
    end;
end;


{ FILE LIST BOX CLICK  }

procedure TMainForm.FileListBox1Click(Sender: TObject);
begin
  if (FileListBox1.ItemIndex <> -1) then
    ShowSummary;                        // If entry clicked, update the summary
end;


procedure TMainForm.FileListBox1DblClick(Sender: TObject);
begin
  if (btnStart.Enabled) then
    btnStartClick(Sender);
end;


{ SHOW SUMMARY }

procedure TMainForm.ShowSummary;
begin                                                         
  DeleteDirectory(AppTempDirectory, True); // Clear temporary folder
  if (UnzipQFiles(FileListBox1.Filename)) then
    begin
      AppQFilename := ExtractFilename(FileListBox1.Filename);
      // Get first entry info (#0) for summary box, inc custom logo if one present
      if (QSet.LoadFromFile(AppTempDirectory + QSET_ZIP_FILENAME)) then
        begin
          memoInfo.Text := QSet.CurrentQuestion.Question;
          if (QSet.CurrentQuestion.Exhibit <> '') then
            LoadLogo(AppTempDirectory + QSet.CurrentQuestion.Exhibit)
          else
            LoadLogo;
          btnStart.Enabled := (QSet.LastIndex > 0);
          btnEdit.Enabled := AdminOptions.IsAddEditEnabled;
          btnDelete.Enabled := AdminOptions.IsDeleteEnabled;
        end
      else
        MessageWarning('Invalid File', 'Cannot open zipped "' + QSET_ZIP_FILENAME + '" file');
    end

  else
    begin
      LoadLogo;
      memoInfo.Clear;
      memoInfo.Lines.Add('"' + AppQFilename + '" HAS UNRECOGNISED FILE FORMAT - CANNOT LOAD');       
      btnStart.Enabled := False;
      btnEdit.Enabled := False;
      btnDelete.Enabled := False;
    end;
end;


{ BUTTON START }

procedure TMainForm.btnStartClick(Sender: TObject);
var
  TimerValue: TTimerValue;
  RunTestForm: TRunTestForm;
begin
  if (FileListBox1.ItemIndex = -1) then
    FileListBox1.ItemIndex := 0;
  if (menuTimeRemaining.Checked) then      // Get timer period?
    begin
      TimerValue := TTimerValue.Create(self);
      try                                      
        TimerValue.TimeLimit := AppTimer;
        if (TimerValue.ShowModal = mrOk) then
          begin
            AppTimer := TimerValue.TimeLimit;
            WriteStatus;
          end;
      finally
        TimerValue.Free;
      end;
    end;

  RunTestForm := TRunTestForm.Create(self);
  try
    RunTestForm.QSetRef := QSet;
    RunTestForm.MainFormLogoRef := imgLogo;
    RunTestForm.Filename := AppQFilename;
    RunTestForm.IsElapsedTime := menuElapsedTime.Checked;
    RunTestForm.ShowModal;
  finally
    RunTestForm.Free;
  end;
end;


{ BUTTON EDIT }

procedure TMainForm.btnEditClick(Sender: TObject);
var
  AddEditForm: TAddEditForm;
begin
  AddEditForm := TAddEditForm.Create(self);
  try
    if (FileListBox1.ItemIndex = -1) then
      FileListBox1.ItemIndex := 0;
    AddEditForm.QSetRef := QSet;
    AddEditForm.ShowModal;
    if (AddEditForm.DataChanged) then
      begin
        QSet.SaveToFile(AppTempDirectory + QSET_ZIP_FILENAME);
        ZipQFiles(GetAppTestsDirectory + AppQFilename);
      end;
  finally
    AddEditForm.Free;
  end;
  ShowSummary;
end;


{ BUTTON ADD }

procedure TMainForm.btnAddClick(Sender: TObject);
var
  Destination: string;
begin
  GetFilename := TGetFilename.Create(self);
  try
    if (GetFileName.ShowModal = mrOk) then
      begin
        AppQFilename := GetFilename.Filename + TESTMASTER_EXT;
        Destination := GetAppTestsDirectory + AppQFilename;
        if FileExists(Destination) then
          if (not MessageQuery('Add Question Set', 'A question set already exists with filename: "' + AppQFilename + '". Overwrite?')) then
            Exit;
                                                                           
        // Create empty QSET in temp folder, zip into new TMQZ in tests folder
        DeleteDirectory(AppTempDirectory, True); // Clear temporary folder
        QSet.Clear(GetFilename.Filename);
        QSet.SaveToFile(AppTempDirectory + QSET_ZIP_FILENAME);
        ZipQFiles(Destination);  
        DeleteDirectory(AppTempDirectory, False); // Delete folder, reopened in FormShow
      end;
  finally
    GetFileName.Free;
    FormShow(Sender);                   // Update buttons and summary
  end;
end;


{ BUTTON DELETE }

procedure TMainForm.btnDeleteClick(Sender: TObject);
begin
  if (MessageQuery('Delete Question Set', 'Are you sure you want to delete "' + ExtractFilename(FileListBox1.Filename) + '"? This cannot be undone.')) then
    begin
      DeleteFile(FileListBox1.Filename);
      FormShow(Sender);                 // Update buttons and summary
    end;
end;


{ BUTTON HELP  }

procedure TMainForm.btnHelpClick(Sender: TObject);
begin
  RunHelp('main.htm');
end;


{ BUTTON EXIT }

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Close;
end;


{ "STATUS" FILE

  I have kept a separate OPT file mainly for the administrator options to
  ensure there is some level of protection from cheating. It is a binary file
  and a little more difficult to 'hack' to change the admin options, say to
  enable the answer button! If a 'hacker' thinks they can get around this by
  simply deleting the file this results in the program creating a new one in a
  locked down status. If it is that important to the 'hacker' it can be
  bypassed as I have also had to set a default password! It is not unreasonable
  to assume an examiner has locked down their PC from user tampering }


{ WRITE STATUS }

procedure TMainForm.WriteStatus;
var
  Stream: TFileStream;
  ByteVar: byte;
begin
  Stream := TFileStream.Create(GetAppDataDirectory(adGlobal) + TESTMASTER_OPT_FILE, fmCreate);
  try
    WriteStr(Stream, TESTMASTER_OPT_VER);
    Stream.Write(menuShowHints.Checked, SizeOf(Boolean));             
    ByteVar := Font.Size;
    Stream.Write(ByteVar, 1);
    Stream.Write(AdminOptions.IsDeleteEnabled, SizeOf(Boolean));
    Stream.Write(AdminOptions.IsAnswersEnabled, SizeOf(Boolean));
    Stream.Write(AdminOptions.IsAddEditEnabled, SizeOf(Boolean));
    Stream.Write(AdminOptions.IsClearResultsEnabled, SizeOf(Boolean));
    Stream.Write(AdminOptions.IsRunningScoreEnabled, SizeOf(Boolean));
    Stream.Write(AdminOptions.PanelColor, SizeOf(Integer));
    WriteStr(Stream, AppPassword);
    Stream.Write(menuElapsedTime.Checked, SizeOf(Boolean));
    Stream.Write(AppTimer, SizeOf(Integer));
  finally
    Stream.Free;
  end;
  ReadStatus;
end;


procedure TMainForm.WriteStr(Stream: TFileStream; Value: string);
var
  StringLength: Integer;
begin
  StringLength := length(Value);
  Stream.Write(StringLength, sizeof(Integer));
  Stream.Write(Value[1], length(value));
end;


{ READ STATUS }

procedure TMainForm.ReadStatus(IsFirst: boolean);
var
  Stream: TFileStream;
  BoolVar: Boolean;
  ByteVar: Byte;
  IntVar: Integer;
  OptVer: string;
  OptFile: string;
  IsFileMissing: boolean;
begin
  OptFile := GetAppDataDirectory(adGlobal) + TESTMASTER_OPT_FILE;
  IsFileMissing := IsFirst and (not FileExists(OptFile));
  if (IsFileMissing) then
    CreateLockedDownStatus
  else
    begin
      Stream := TFileStream.Create(OptFile, fmOpenRead);
      try
        ReadStr(Stream, OptVer{%H-});
        if (OptVer <> TESTMASTER_OPT_VER) then
          begin
            MessageWarning('Error',  '"' + TESTMASTER_OPT_FILE + '" file does not appear to be valid for this version of TestMaster. Locking down and overwriting');
            CreateLockedDownStatus;
          end
        else
          begin
            Stream.Read(BoolVar{%H-}, SizeOf(Boolean));
            menuShowHints.Checked := BoolVar;
            Stream.Read(ByteVar{%H-}, 1);
            Font.Size := ByteVar;
            Stream.Read(BoolVar, SizeOf(Boolean));
            AdminOptions.IsDeleteEnabled := BoolVar;
            Stream.Read(BoolVar, SizeOf(Boolean));
            AdminOptions.IsAnswersEnabled := BoolVar;
            Stream.Read(BoolVar, SizeOf(Boolean));
            AdminOptions.IsAddEditEnabled := BoolVar;
            Stream.Read(BoolVar, SizeOf(Boolean));
            AdminOptions.IsClearResultsEnabled := BoolVar;
            Stream.Read(BoolVar, SizeOf(Boolean));
            AdminOptions.IsRunningScoreEnabled := BoolVar;
            Stream.Read(IntVar{%H-}, SizeOf(Integer));
            AdminOptions.PanelColor := IntVar;
            ReadStr(Stream, AppPassword);
            Stream.Read(BoolVar, SizeOf(Boolean));
            menuElapsedTime.Checked := BoolVar;
            menuTimeRemaining.Checked := not menuElapsedTime.Checked;
            Stream.Read(IntVar, SizeOf(Integer));
            AppTimer := IntVar;             //  Global var
          end;
      finally
        Stream.Free;
      end;
    end;
  //
  case Font.Size of
      8: menuFontSizeSmall.Checked := True;
     10: menuFontSizeMedium.Checked := True;
     12: menuFontsizeLarge.Checked := True;
  end;
  ShowHint := menuShowHints.Checked;     
  AppFontSize := Font.Size;             //  Global var
  AppShowHint := ShowHint;              //  Global var
  menuAdministrator.Visible := AdminOptions.IsAddEditEnabled;
  N3.Visible := AdminOptions.IsAddEditEnabled;
  btnDelete.Enabled := AdminOptions.IsDeleteEnabled;
  btnEdit.Visible := AdminOptions.IsAddEditEnabled;
  btnAdd.Visible := AdminOptions.IsAddEditEnabled;
  btnDelete.Visible := AdminOptions.IsAddEditEnabled;
  if (AdminOptions.IsAddEditEnabled) then
    lblDropHelp.Left := btnDelete.Left + 68
  else
    lblDropHelp.Left := btnStart.Left + 68 + 16; // Start button is 16px wider
  //
  if (IsFileMissing or (OptVer <> TESTMASTER_OPT_VER)) then
    WriteStatus;
end;


procedure TMainForm.ReadStr(Stream: TFileStream; var Value: string);
var
  StringLength: Integer;
begin
  Stream.Read(StringLength{%H-}, sizeof(Integer));
  SetLength(Value, StringLength);
  Stream.Read(Value[1], StringLength);
end;


procedure TMainForm.CreateLockedDownStatus;
begin
  menuShowHints.Checked := True;
  AdminOptions.IsDeleteEnabled := False;
  AdminOptions.IsAnswersEnabled := False;
  AdminOptions.IsAddEditEnabled := False;
  AdminOptions.IsClearResultsEnabled := False;
  Font.Size := 10;
  AdminOptions.PanelColor := clDefault;
  AppPassword := Encrypt(DEFAULT_ADMIN_PW);
  AdminOptions.IsRunningScoreEnabled := False;
  menuElapsedTime.Checked := True;
  menuTimeRemaining.Checked := not menuElapsedTime.Checked;
  AppTimer := 60;
end;


{ MENU RESULTS }

procedure TMainForm.MenuResultsClick(Sender: TObject);
begin
  ResultsForm.ShowModal;
end;


{ MENU TESTMASTER HELP }

procedure TMainForm.menuTestmasterHelpClick(Sender: TObject);
begin
  RunHelp('start.htm');
end;


{ MENU ABOUT }

procedure TMainForm.menuAboutWindowsClick(Sender: TObject);
var
  AboutBox: TAboutBox;
begin
  AboutBox := TAboutBox.Create(self);
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;


{ MENU SHOW HINTS }

procedure TMainForm.menuShowHintsClick(Sender: TObject);
begin
  menuShowHints.Checked := not menuShowHints.Checked;
  WriteStatus;
end;


{ MENU FONT SIZE }

procedure TMainForm.FontSizeClick(Sender: TObject);
var
  ThisMenuItem: TMenuItem;
begin
  ThisMenuItem := (Sender as TMenuItem);
  ThisMenuItem.Checked := True;
  Font.Size := ThisMenuItem.Tag;
  AppFontSize := Font.Size;
  WriteStatus;
end;


{ MENU ELAPSED TIME / TIME REMAINING }

{ The 'menuElapsedTime' and 'menuTimeRemaining' items are part of a RadioItem
  Group (index=2) so setting one automatically unchecks the other }

procedure TMainForm.ElapsedTimeClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  WriteStatus;
end;


{ MENU ADMINISTRATOR }

{ If the Add/Edit buttons are disabled it assumes you're running a real test
  scenario so Administrator option is disabled. However should an Admin want to
  access the Options menu, pressing Ctrl-A will achieve this. Still password
  protected of course }

procedure TMainForm.menuAdministratorClick(Sender: TObject);
begin
  DoAdministrator;
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (not AdminOptions.IsAddEditEnabled) and (Key = ord('A')) and (ssCtrl in Shift) then
    DoAdministrator;
end;


procedure TMainForm.DoAdministrator;
var
  PasswordDlg: TGetPasswordDlg;
begin
  PasswordDlg := TGetPasswordDlg.Create(self);
  try
    if (PasswordDlg.ShowModal = mrOk) then
      begin
        if (AdminOptions.ShowModal = mrOk) then
          WriteStatus
        else
          ReadStatus;
      end;
  finally
    PasswordDlg.Free;
  end;
end;


{ ON DROP FILES }

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i, Count: integer;
  FromFilename, ToFilename: string;
begin
  Count := 0;
  for i := 0 to (Length(FileNames) - 1) do
    begin
      FromFilename := FileNames[i];
      ToFilename := GetAppTestsDirectory + ExtractFilename(FromFilename);
      if (FromFilename <> '') and (Pos(LowerCase(ExtractFileExt(FromFilename)), DROP_EXTS) > 0) then
        begin
          CopyFile(FromFilename, ToFilename);
          Inc(Count);
        end
      else
        begin
          MessageWarning('Drop Question Set', 'Test file must have extension TMQZ');
          Exit;
        end;
      MessageInfo(Format('%d files copied to "%s"',  [Count, GetAppTestsDirectory]));
    end;
  FileListBox1.UpdateFileList;
end;


{ RESIZE }

procedure TMainForm.FormResize(Sender: TObject);
var
  UnitWidth: integer;
begin
  { 3 = 1 part FileListBox, 2 parts Summary box; 24 = space around these }
  UnitWidth := trunc((Width - 24) / 3);
  if (UnitWidth < FILELISTBOX_MAX_WIDTH) then
    FileListBox1.Width := UnitWidth
  else
    FileListBox1.Width := FILELISTBOX_MAX_WIDTH;
end;


end.
