{ ==============================================================================

  ADD / EDIT FORM

    This form provides the facility to edit question sets, adding / editing /
    deleting as required. A find option allows a search for specific text
    within the question set

    Interface:
    - QSetRef; property assignment for reference to the main question set
    - DataChanged; indicates when the question set data has changed allowing
                   the caller to save it


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

unit uAddEditForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls,
  Graphics, FileUtil,
  //
  uSelectExhibitForm, uShowExhibitForm, uQuestionSet, uSearchForm, uJsonCfgFile,
  uCommon;

type
  TState = (tsView, tsEdit, tsAdd);

  { TAddEditForm }

  TAddEditForm = class(TForm)
    btnExhibitAdd: TButton;
    btnExhibitDelete: TButton;
    btnExhibitShow: TButton;
    btnFind: TButton;
    btnFirst: TButton;
    btnLast: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    cbRandomize: TCheckBox;
    gbAnswers: TGroupBox;
    gbExhibit: TGroupBox;
    gbNavigation: TGroupBox;
    gbQuestion: TGroupBox;
    gbQuestionNumber: TGroupBox;
    gbRemarks: TGroupBox;
    gbAddEtc: TGroupBox;
    imgExhibit: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblTotalQ: TLabel;
    lblQuestionNumber: TLabel;
    memoOption1: TMemo;
    memoOption2: TMemo;
    memoOption3: TMemo;
    memoOption4: TMemo;
    memoOption5: TMemo;
    memoOption6: TMemo;
    memoOption7: TMemo;
    memoOption8: TMemo;
    memoQuestion: TMemo;
    memoRemarks: TMemo;
    panelLeft: TPanel;
    panelButtons: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnSave: TButton;
    btnCancel: TButton;
    panelExhibit: TPanel;
    panelTop: TPanel;
    rgAnswerType: TRadioGroup;
    btnClose: TButton;
    btnHelp: TButton;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure SpeedButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnExhibitAddClick(Sender: TObject);
    procedure btnExhibitDeleteClick(Sender: TObject);
    procedure btnExhibitShowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure memoQuestionChange(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure rgAnswerTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    fQSetRef: TQuestionSet;                                                           
    fDataChanged: Boolean;
    EditState: TState;
    UpdatingFlag: Boolean;  // Allows question display without MemoChange activating
    TempQ: TQuestion;
    localAnswer: Byte;      // Each bit represents answer option 0-7
    procedure ShowTotalQ;
  public
    localExhibit: string;
    procedure SetQuestion(Number: integer);
    procedure SetEdit(State: TState);
    procedure ShowQuestion;
    procedure ShowAnswerButtons;
    procedure ShowExhibit;
    function  ShowImage: boolean;
    //
    property QSetRef: TQuestionSet read fQSetRef write fQSetRef;
    property DataChanged: boolean read fDataChanged;
  end;


implementation

{$R *.lfm}   

const
  SECT_CFG = 'EditForm';
  DROP_EXTENSIONS = '*.bmp;*.jpg;*.png';

var
  SearchForm: TSearchForm;              // Non-modal form so variable here


{ CREATE }

procedure TAddEditForm.FormCreate(Sender: TObject);
begin
  Left := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_LEFT, 40);
  Top := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_TOP, 40);
  Width := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_WIDTH, 0);
  Height := AppCfg.ReadInteger(SECT_CFG, CFG_WDW_HEIGHT, 0);
  //
  SearchForm := TSearchForm.Create(self);
end;


{ SHOW }

procedure TAddEditForm.FormShow(Sender: TObject);
begin
  SearchForm.QSetRef := fQSetRef;       // Setup search form parameters
  SearchForm.OnFind := @ShowQuestion;
  SearchForm.Top := self.Top + gbNavigation.Top + 180;
  SearchForm.Left := self.Left + panelButtons.Left - 132;
  //
  // Font.Size := AppFontSize;    { Keep smaller size for editing }
  ShowHint := AppShowHint;
  Caption := 'TestMaster - Add / Edit Questions [' + AppQFilename + ']';
  UpdatingFlag := False;
  fDataChanged := False;
  SetEdit(tsView);
  SetQuestion(0);
  ShowTotalQ;
  FormResize(nil);                      //  Force resize display elements
end;


procedure TAddEditForm.ShowTotalQ;
begin
  lblTotalQ.Caption := 'Total: ' + IntToStr(fQSetRef.LastIndex);
end;


{ FORM CLOSE }

procedure TAddEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not btnClose.Enabled) then
    begin
      MessageWarning('Cannot close window', 'You must Save or Cancel edit before closing window');
      CloseAction := caNone;
    end;
end;


{ DESTROY }

procedure TAddEditForm.FormDestroy(Sender: TObject);
begin
  SearchForm.Free;
  //
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_LEFT, Left);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_TOP, Top);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_WIDTH, Width);
  AppCfg.WriteInteger(SECT_CFG, CFG_WDW_HEIGHT, Height);
end;       


{ SET EDIT STATE }

procedure TAddEditForm.SetEdit(State: TState);
begin
  EditState := State;
  // Set buttons according to edit state
  btnSave.Enabled   := EditState in [tsEdit, tsAdd];
  btnCancel.Enabled := btnSave.Enabled;
  btnAdd.Enabled    := not btnSave.Enabled;
  btnDelete.Enabled := (not btnSave.Enabled) and (fQSetRef.CurrentIndex <> 0);
  btnClose.Enabled  := not btnSave.Enabled;
  // Set navigation buttons
  btnFirst.Enabled  := (not btnSave.Enabled) and (fQSetRef.CurrentIndex > 0);
  btnPrev.Enabled   := btnFirst.Enabled;
  btnNext.Enabled   := (not btnSave.Enabled) and (fQSetRef.CurrentIndex < fQSetRef.LastIndex);
  btnLast.Enabled   := btnNext.Enabled;
  // Set search button
  btnFind.Enabled   := not btnSave.Enabled;
end;


{ SHOW QUESTION }

procedure TAddEditForm.ShowQuestion;
begin
  UpdatingFlag := True;                 // Let memochange know its updating
  lblQuestionNumber.Caption := IntToStr(fQSetRef.CurrentIndex);
  with fQSetRef.CurrentQuestion do
  begin                                 // Set all local values and show them
    rgAnswerType.ItemIndex := QuestionType;
    cbRandomize.Checked := Randomize;
    memoQuestion.Text := Question;
    memoOption1.Text  := Option1;
    memoOption2.Text  := Option2;
    memoOption3.Text  := Option3;
    memoOption4.Text  := Option4;
    memoOption5.Text  := Option5;
    memoOption6.Text  := Option6;
    memoOption7.Text  := Option7;
    memoOption8.Text  := Option8;
    memoRemarks.Text  := Remarks;
    localAnswer       := Answer;
    localExhibit      := Exhibit;
    ShowAnswerButtons;
    ShowExhibit;
  end;
  UpdatingFlag := False;
end;   


{ SHOW ANSWER BUTTONS }

procedure TAddEditForm.ShowAnswerButtons;
var
  i: Integer;
begin
  for i := 0 to (ComponentCount - 1) do
    if (Components[i] is TSpeedButton) then
      if ( (localAnswer and TSpeedButton(Components[i]).Tag) > 0 ) then
        TSpeedButton(Components[i]).Caption := 'X'
      else
        TSpeedButton(Components[i]).Caption := '';
end;


{ SHOW EXHIBIT }

procedure TAddEditForm.ShowExhibit;
begin                             
  panelExhibit.Color := clWhite;
  if (localExhibit <> '') then          // Update image, if there is one
    begin
      btnExhibitShow.Enabled   := ShowImage;
      btnExhibitDelete.Enabled := True;
      btnExhibitAdd.Caption    := 'Change';
    end
  else
    begin
      imgExhibit.Picture.Clear;
      panelExhibit.Caption := 'No Exhibit';
      btnExhibitDelete.Enabled := False;
      btnExhibitShow.Enabled   := False;
      btnExhibitAdd.Caption    := 'Add';
    end;
end;


function TAddEditForm.ShowImage: boolean;
var
  ThisFilename: string;
  ImageFound: boolean;
begin
  ThisFilename := AppTempDirectory + localExhibit;
  ImageFound := LoadImage(imgExhibit, ThisFilename);

  if (ImageFound) then
    begin
      FitImage(imgExhibit);
      panelExhibit.Caption := '';
    end
  else
    begin                       
      imgExhibit.Picture.Clear;
      panelExhibit.Color := clYellow;   // Highlight missing file
      btnExhibitShow.Enabled := False;
      panelExhibit.Caption := 'Exhibit not found: ' + CRLF + localExhibit;
    end;
  Result := ImageFound;
end;


{ MEMO QUESTION ONCHANGE }

procedure TAddEditForm.memoQuestionChange(Sender: TObject);
begin
  // If in view mode and a change has been made, goto edit mode
  if ( (EditState = tsView) and (not UpdatingFlag) ) then
    SetEdit(tsEdit);
end;    


{ BUTTON EXHIBIT ADD }

procedure TAddEditForm.btnExhibitAddClick(Sender: TObject);
var
  ExhibitManager: TExhibitManager;
begin
  ExhibitManager := TExhibitManager.Create(self);
  ExhibitManager.ShowHint := AppShowHint;
  ExhibitManager.FilePath := AppTempDirectory;
  ExhibitManager.Filename := localExhibit;
  try
    if (ExhibitManager.ShowModal = mrOk) then
      begin
        localExhibit := ExtractFilename(ExhibitManager.FileName);
        if (EditState = tsView) then
          SetEdit(tsEdit);              // If in view mode, set to edit mode
      end;
  finally
    ExhibitManager.Free;
  end;
  ShowExhibit;                          // Ensure any changes reflected
end;


{ BUTTON EXHIBIT DELETE }

procedure TAddEditForm.btnExhibitDeleteClick(Sender: TObject);
begin
  if (MessageQuery('Delete Exhibit', 'Are you sure you want to delete this exhibit?')) then
    begin
      localExhibit := '';
      if (EditState = tsView) then
        SetEdit(tsEdit);                // If viewing then put in edit mode
      ShowExhibit;
    end;
end;


{ BUTTON EXHIBIT SHOW }

procedure TAddEditForm.btnExhibitShowClick(Sender: TObject);
var
  ShowForm: TShowExhibitForm;
begin
  ShowForm := TShowExhibitForm.Create(self);
  try
    ShowForm.ExhibitFile := AppTempDirectory + localExhibit;
    ShowForm.ShowModal;
  finally
    ShowForm.Free;
  end;
end;


{ SPEED BUTTON - mark option answers appropriately }

procedure TAddEditForm.SpeedButtonClick(Sender: TObject);
var
  ThisButton: TSpeedButton;
begin
  ThisButton := (Sender as TSpeedButton);
  if (rgAnswerType.ItemIndex = 0) then  // Single choice ?
    localAnswer := ThisButton.Tag
  else
    localAnswer := localAnswer xor ThisButton.Tag;
  if (EditState = tsView) then
    SetEdit(tsEdit);                    // If viewing then put in edit mode
  ShowAnswerButtons;                    // and update display
end;


{ BUTTON ADD QUESTION }

procedure TAddEditForm.btnAddClick(Sender: TObject);
begin
  if (fQSetRef.LastIndex < 250) then
    begin
      SetEdit(tsAdd);
      rgAnswerType.ItemIndex := 0;      // Set empty question
      cbRandomize.Checked := True;
      memoQuestion.Text := '';
      memoOption1.Text  := '';
      memoOption2.Text  := '';
      memoOption3.Text  := '';
      memoOption4.Text  := '';
      memoOption5.Text  := '';
      memoOption6.Text  := '';
      memoOption7.Text  := '';
      memoOption8.Text  := '';
      memoRemarks.Text  := '';
      localAnswer       := 0;
      localExhibit      := '';
      ShowExhibit;                      // Clear any existing exhibit
      ShowAnswerButtons;
      lblQuestionNumber.Caption := IntToStr(fQSetRef.LastIndex+1);
      memoQuestion.SetFocus;
    end
  else
    MessageWarning('Add Question',  'Limit of 250 questions reached');
end;


{ BUTTON DELETE QUESTION }

procedure TAddEditForm.btnDeleteClick(Sender: TObject);
begin
  if (MessageQuery('Delete Question', 'Are you sure you want to delete this question?')) then
    begin                                                                     
      fDataChanged := True;
      fQSetRef.Delete;
      ShowTotalQ;
      SetQuestion(fQSetRef.CurrentIndex); // Show next entry and set buttons
    end;
end;     


{ BUTTON SAVE QUESTION }

procedure TAddEditForm.btnSaveClick(Sender: TObject);
begin
  fDataChanged := True;                // Mark flag so memory array saved
  with TempQ do                        // Save all local variables to QSet
  begin
    QuestionType := rgAnswerType.ItemIndex;
    Randomize    := cbRandomize.Checked;
    Question := memoQuestion.Text;
    Option1  := memoOption1.Text;
    Option2  := memoOption2.Text;
    Option3  := memoOption3.Text;
    Option4  := memoOption4.Text;
    Option5  := memoOption5.Text;
    Option6  := memoOption6.Text;
    Option7  := memoOption7.Text;
    Option8  := memoOption8.Text;
    Remarks  := memoRemarks.Text;
    Answer   := localAnswer;
    Exhibit  := localExhibit;
  end;
  if (EditState = tsAdd) then
    fQSetRef.Add(TempQ)
  else
    fQSetRef.Update(TempQ);
  SetEdit(tsView);
  ShowTotalQ;
end;


{ BUTTON CANCEL }

procedure TAddEditForm.btnCancelClick(Sender: TObject);
begin
  SetEdit(tsView);
  ShowTotalQ;
  SetQuestion(fQSetRef.CurrentIndex);   // Restore original entry and buttons
end;


{ ANSWER TYPE ONCLICK }

procedure TAddEditForm.rgAnswerTypeClick(Sender: TObject);
var
  i: Integer;
begin
  // Ensure that if going from multiple to single that any multiple reduced to one
  if (rgAnswerType.ItemIndex = 0) then
    begin
      for i := 0 to (ComponentCount - 1) do
        if (Components[i] is TSpeedButton) then
          if ( (localAnswer and TSpeedButton(Components[i]).Tag) > 0 ) then
            begin
              localAnswer := TSpeedButton(Components[i]).Tag;
              break;
            end;
      ShowAnswerButtons;
    end;
  // If in view mode and a change has been made, goto edit mode
  if ( (EditState = tsView) and (not UpdatingFlag) and (rgAnswerType.ItemIndex <> fQSetRef.CurrentQuestion.QuestionType) ) then
    SetEdit(tsEdit);
end;         


{ NAVIGATION BUTTONS }

procedure TAddEditForm.btnFirstClick(Sender: TObject);
begin
  SetQuestion(0);
end;


procedure TAddEditForm.btnPrevClick(Sender: TObject);
begin
  if (fQSetRef.CurrentIndex > 0) then
    SetQuestion(fQSetRef.CurrentIndex - 1);
end;


procedure TAddEditForm.btnNextClick(Sender: TObject);
begin
  if (fQSetRef.CurrentIndex < fQSetRef.LastIndex) then
    SetQuestion(fQSetRef.CurrentIndex + 1);
end;


procedure TAddEditForm.btnLastClick(Sender: TObject);
begin
  SetQuestion(fQSetRef.LastIndex);
end;


{ BUTTON FIND }

procedure TAddEditForm.btnFindClick(Sender: TObject);
begin
  SearchForm.Show;                      // Non modal so can select edit window
end;   


{ BUTTON HELP }

procedure TAddEditForm.btnHelpClick(Sender: TObject);
begin
  RunHelp('edit.htm');
end;


{ BUTTON CLOSE }

procedure TAddEditForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


{ SET QUESTION }

procedure TAddEditForm.SetQuestion(Number: Integer);
begin
  fQSetRef.CurrentIndex := Number;      // Set current question number
  SetEdit(EditState);                   // Update navigation buttons
  ShowQuestion;
end; 


{ ON DROP FILES }

procedure TAddEditForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  FromFilename, ToFilename: string;
begin
  if (Length(FileNames) > 1) then
    MessageWarning('Drop Exhibit', 'Only one image file can be dropped here')
  else
    begin
      FromFilename := FileNames[0];
      ToFilename := AppTempDirectory + ExtractFilename(FromFilename);
      if (FromFilename <> '') and (Pos(LowerCase(ExtractFileExt(FromFilename)), DROP_EXTENSIONS) > 0) then
        begin
          CopyFile(FromFilename, ToFilename);
          localExhibit := ExtractFilename(FromFilename);
          if (EditState = tsView) then
            SetEdit(tsEdit);            // If in view mode, set to edit mode
          ShowExhibit;                  // Ensure any changes reflected
        end
      else
        MessageWarning('Drop Exhibit', 'Image file must be BMP, JPG, or PNG');
    end;
end;


{ FORM RESIZE }

procedure TAddEditForm.FormResize(Sender: TObject);
var
  UnitHeight, AnswerWidth: integer;
begin
  // 40 = top bar + gaps between items
  // 7 = 2 units for gbQuestion, 3 units for gbAnswers, 2 units for gbRemarks
  UnitHeight := trunc((Height - 40) / 7);
  gbAnswers.Height := UnitHeight * 3;
  if (gbAnswers.Height > 320) then
    begin
      // Only expand until all 8 answer rows shown
      gbAnswers.Height := 320;
      // Divide up the remainder as 4 units (gbQuestion + gbRemarks)
      UnitHeight := (Height - 40 - 320) div 4;
    end;
  gbRemarks.Height := UnitHeight * 2;
  // No need to set gbQuestion height as set to alClient and will autosize
  gbExhibit.Width := panelExhibit.Height + 93;
  AnswerWidth := gbAnswers.Width - 86;
  memoOption1.Width := AnswerWidth;
  memoOption2.Width := AnswerWidth;
  memoOption3.Width := AnswerWidth;
  memoOption4.Width := AnswerWidth;
  memoOption5.Width := AnswerWidth;
  memoOption6.Width := AnswerWidth;
  memoOption7.Width := AnswerWidth;
  memoOption8.Width := AnswerWidth;
end;


end.
