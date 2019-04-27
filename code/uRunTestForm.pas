{ ==============================================================================

  RUN TEST FORM

  This form executes the selected test, maintaining a running total of the
  correct answers. If enabled users can preview the correct answers, mark
  the current question to come back to later and offers a sidebar to jump
  easily between questions

    Interface:
    - QSetRef
    - MainFormLogoRef
    - Filename
    - IsElapsedTime


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

unit uRunTestForm;

{$mode objfpc}{$h+}

interface

uses
  LCLType, LCLIntf, Graphics, Classes, SysUtils, Controls, Forms, Menus,
  Dialogs, ExtCtrls, StdCtrls, Math,
  //
  uResultsForm, uOptionsForm, uQuestionSet, uJsonCfgFile, uCommon;

type
  TCheck = record
    Answer: Byte;
    UserAnswer: Byte;
    Viewed: Boolean;                    // Used to check if question has been viewed
    Correct: Boolean;
    Marked: Boolean;
  end;

  { TRunTestForm }

  TRunTestForm = class(TForm)
    panelLeft: TPanel;
    panelExhibit: TPanel;
    TestTimer: TTimer;
    panelButtons: TPanel;
    ScrollBox1: TScrollBox;
    lblQuestion: TLabel;
    Line1: TPanel;
    cb1: TCheckBox;
    cb2: TCheckBox;
    cb3: TCheckBox;
    cb4: TCheckBox;
    cb5: TCheckBox;
    cb6: TCheckBox;
    cb7: TCheckBox;
    cb8: TCheckBox;
    id1: TLabel;
    id2: TLabel;
    id3: TLabel;
    id4: TLabel;
    id5: TLabel;
    id6: TLabel;
    id7: TLabel;
    id8: TLabel;
    cbl1: TLabel;
    cbl2: TLabel;
    cbl3: TLabel;
    cbl4: TLabel;
    cbl5: TLabel;
    cbl6: TLabel;
    cbl7: TLabel;
    cbl8: TLabel;
    rb1: TRadioButton;
    rb2: TRadioButton;
    rb3: TRadioButton;
    rb4: TRadioButton;
    rb5: TRadioButton;
    rb6: TRadioButton;
    rb7: TRadioButton;
    rb8: TRadioButton;
    Line2: TPanel;
    panelTop: TPanel;
    lbTitle: TLabel;
    btnNext: TButton;
    btnPrevious: TButton;
    btnEndReview: TButton;
    btnClose: TButton;
    LogoImage: TImage;
    lblResults: TLabel;
    btnMark: TButton;
    lbTime: TLabel;
    imgExhibit: TImage;
    lbReference: TLabel;
    ListBox1: TListBox;
    Line3: TPanel;
    btnSave: TButton;
    btnHelp: TButton;
    btnAnswer: TButton;
    btnGoto: TButton;
    procedure cbl1Click(Sender: TObject);
    procedure cbl2Click(Sender: TObject);
    procedure cbl3Click(Sender: TObject);
    procedure cbl4Click(Sender: TObject);
    procedure cbl5Click(Sender: TObject);
    procedure cbl6Click(Sender: TObject);
    procedure cbl7Click(Sender: TObject);
    procedure cbl8Click(Sender: TObject);
    procedure btnAnswerClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TestTimerTimer(Sender: TObject);
    procedure btnEndReviewClick(Sender: TObject);
    procedure btnMarkClick(Sender: TObject);
    procedure btnGotoClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    fQSetRef: TQuestionSet;
    fMainFormLogoRef: TImage;
    fFilename: string;
    fIsElapsedTime: boolean;
    RandomOrder: boolean;
    NextFreeY: integer;
    TestTime: integer;
    OldNext, OldPrev, OldEnd, OldClose: boolean;
    MaxNumber: integer;
    CurrentQuestion: integer;
    CheckArray: array[1..250] of TCheck;
    QuestionOrder: array[1..250] of byte;
    AnswerOrder: array[1..250, 1..8] of byte;
    ReviewFlag: boolean;
    AnswerFlag: boolean;
    ErrorsFlag: boolean;
    IsClosePressed: boolean;
    function CalcHeight(aWidth: integer; aLabel: TLabel): integer;
    procedure OrderQuestions;
    procedure OrderAnswers(NumOpts: byte);
    function  RealQuestionNumber: integer;
    procedure SetFontAndHints;
    procedure SetQuestion(Number: integer);
    procedure SetSize(aLabel: TLabel; aLeftMargin: integer);
    procedure ShowQuestion;
    procedure ShowExhibit;
    function  GetWidth(LeftMargin: integer): integer;
    procedure ShowAnswer(Answer: byte);
    procedure ShowAnswerChoices;
    function  Power2(Exp: integer): byte;
    procedure ShowItem(refButton:TRadioButton; refBox:TCheckBox; refID:TLabel; refText:TLabel; refValue: string);
    procedure ShowCorrectAnswer(IsShowing: boolean);
    procedure CheckAnswer;
    procedure EvaluateResults(CheckReview: boolean);
    procedure ReOrderErrors;
    function  ShowTime(Seconds: integer): string;
    procedure StartTimer;
  public                                   
    property QSetRef: TQuestionSet write fQSetRef;
    property MainFormLogoRef: TImage write fMainFormLogoRef;  
    property Filename: string write fFilename;
    property IsElapsedTime: boolean write fIsElapsedTime;
end;


implementation

{$R *.lfm}

const
  SECT_CFG = 'RunTestForm';


{ CREATE }

procedure TRunTestForm.FormCreate(Sender: TObject);
begin
  Left := AppCfg.Readinteger(SECT_CFG, CFG_WDW_LEFT, 40);
  Top := AppCfg.Readinteger(SECT_CFG, CFG_WDW_TOP, 40);
  Width := AppCfg.Readinteger(SECT_CFG, CFG_WDW_WIDTH, 0);
  Height := AppCfg.Readinteger(SECT_CFG, CFG_WDW_HEIGHT, 0);
  //
  fQSetRef := nil;                      { TODO : Check if these not set externally, form still works ok }
  fMainFormLogoRef := nil;
  fFilename := '';
  fIsElapsedTime := True;
end;
       

{ DESTROY }

procedure TRunTestForm.FormDestroy(Sender: TObject);
begin  
  AppCfg.Writeinteger(SECT_CFG, CFG_WDW_LEFT, Left);
  AppCfg.Writeinteger(SECT_CFG, CFG_WDW_TOP, Top);
  AppCfg.Writeinteger(SECT_CFG, CFG_WDW_WIDTH, Width);
  AppCfg.Writeinteger(SECT_CFG, CFG_WDW_HEIGHT, Height);
end;


{ SHOW }

procedure TRunTestForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  LogoImage.Picture := fMainFormLogoRef.Picture;
  panelTop.Height := Min(120, LogoImage.Picture.Height); // Max logo height = 120
  FormResize(nil);                                       // Force resize adjustments
  //
  btnAnswer.Visible := AdminOptions.IsAnswersEnabled;  
  ScrollBox1.Color := AdminOptions.PanelColor;
  // 
  Font.Size := AppFontSize;
  ShowHint := AppShowHint;

  panelTop.Color := LogoImage.Picture.Bitmap.Canvas.Pixels[0,0]; // Colour match to left of logo

  Caption := 'TestMaster [' + fFilename + ']';
  ListBox1.Visible := False;
  // Initial button states set in designer; btnNext/btnPrevious set by SetQuestion below
  IsClosePressed     := False;          // Used to trap [X] close button
  lblResults.Caption := '';
  ReviewFlag         := False;
  AnswerFlag         := False;
  fQSetRef.CurrentIndex := 0;
  RandomOrder := ((fQSetRef.CurrentQuestion.Answer and 1) = 1);
  MaxNumber := fQSetRef.LastIndex;
  for i := 1 to MaxNumber do
    AnswerOrder[i,1] := 0;              // Set order as not set
  OrderQuestions;
  SetQuestion(1);                       // Start at first question
  StartTimer;
end;


{ FORM CLOSE }

procedure TRunTestForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (IsClosePressed) then
    begin
      if (btnSave.Enabled and (MessageQuery('Save Results', 'Do you want to save the results?'))) then
        btnSaveClick(Sender);
    end
  else
    begin
      MessageWarning('Cannot close window',  'End Test and select Close button to exit');
      CloseAction := caNone;
    end;
end;


{ ORDER QUESTIONS }

{ If random order selected this is achieved by progressively by exchanging a
  random entry with the highest entry and then the range of random numbers is
  reduced by one. Random order builds from highest entry downwards }

procedure TRunTestForm.OrderQuestions;
var
  i, temp, NextRandom: integer;
begin
  for i := 1 to MaxNumber do
    begin
      QuestionOrder[i] := i;            // Put in straight order first
      CheckArray[i].UserAnswer := 0;    // Clear any previous answers
      CheckArray[i].Correct := True;
      CheckArray[i].Marked := False;
      CheckArray[i].Viewed := False;
    end;

  if (RandomOrder) then
    begin
      Randomize;
      for i := MaxNumber downto 2 do
        begin
          NextRandom := Random(i) + 1;  // Ensure in range 1 to 'i'
          temp := QuestionOrder[i];
          QuestionOrder[i] := QuestionOrder[NextRandom];
          QuestionOrder[NextRandom] := temp;
        end;
    end;
end;


{ REAL QUESTION ORDER }

{ Returns actual question number based on QuestionOrder array }

function TRunTestForm.RealQuestionNumber: integer;
begin
  Result := QuestionOrder[CurrentQuestion];
end;


{ SET CURRENT QUESTION NUMBER }

procedure TRunTestForm.SetQuestion(Number: integer);
begin
  CurrentQuestion := Number;            // Set current question number
  btnPrevious.Enabled := (CurrentQuestion > 1); // update buttons as required
  btnNext.Enabled := (CurrentQuestion < MaxNumber);
  ShowQuestion;                         // and display the question
end;


{ SHOW QUESTION }

procedure TRunTestForm.ShowQuestion;
var
  NumString: string;
begin
  fQSetRef.CurrentIndex := RealQuestionNumber;
  CheckArray[RealQuestionNumber].Viewed := True;
  // Show any answer user has made, if already answered
  ShowAnswer(CheckArray[RealQuestionNumber].UserAnswer);

  if (ReviewFlag) then
    NumString := IntToStr(RealQuestionNumber)
  else
    NumString := IntToStr(CurrentQuestion) + ' of ' + IntToStr(MaxNumber);

  if (CheckArray[RealQuestionNumber].Marked) then
    begin
      NumString := NumString + ' (Marked)';
      btnMark.Caption := 'Unmark';
    end
  else
    btnMark.Caption := 'Mark';

  lbTitle.Caption := 'Question ' + NumString;
  ScrollBox1.VertScrollBar.Position := 0; // Ensure scroll box at top
  NextFreeY := 16;                      // Initialise drawing point (top margin)
  ShowExhibit;
  lblQuestion.Top := NextFreeY;
  lblQuestion.Caption := fQSetRef.CurrentQuestion.Question;
  SetSize(lblQuestion, 16 {= left margin});
  NextFreeY := NextFreeY + lblQuestion.Height + 12;
  Line1.Top := NextFreeY;
  Line1.Width := GetWidth(16);
  NextFreeY := NextFreeY + Line1.Height + 12;
  ShowAnswerChoices;
  Line2.Top := NextFreeY;
  Line2.Width := GetWidth(16);          // Keep checking width of any exhibit
  NextFreeY := NextFreeY + Line2.Height + 12;
  ShowCorrectAnswer(AnswerFlag or ReviewFlag);
  NextFreeY := Max(NextFreeY, imgExhibit.Height + 32 {= top + bottom margins});
  Line3.Width := GetWidth(16);
  Line3.Top := NextFreeY;               // Ensure gap at bottom of scroll box
end;


procedure TRunTestForm.ShowAnswer(Answer: Byte);
var
  i: integer;
begin
  // Check buttons/boxes according to answer passed
  if (fQSetRef.CurrentQuestion.QuestionType = 0) then
    begin
      for i := 0 to ComponentCount - 1 do
        if (Components[i] is TRadioButton) then
          TRadioButton(Components[i]).Checked := (TRadioButton(Components[i]).Tag and Answer) > 0;
    end
  else
    begin
      for i := 0 to ComponentCount - 1 do
        if (Components[i] is TCheckBox) then
          TCheckBox(Components[i]).Checked := (TCheckBox(Components[i]).Tag and Answer) > 0;
    end;
end;


procedure TRunTestForm.ShowExhibit;
var
  MaxWidth: integer;
begin
  if (fQSetRef.CurrentQuestion.Exhibit <> '') then
    begin                    
      panelExhibit.Visible := True;
      if ( LoadImage(imgExhibit, AppTempDirectory + fQSetRef.CurrentQuestion.Exhibit) ) then
        begin
          MaxWidth := ScrollBox1.Width - 16 - 24;
          if (imgExhibit.Picture.Width > (2 * imgExhibit.Picture.Height)) then
            begin                       // If wide, show at the top full width
              panelExhibit.Left := 16;
              imgExhibit.Width := MaxWidth;
              FitImage(imgExhibit, True);
              NextFreeY := NextFreeY + imgExhibit.Height + 12;
            end
          else
            begin
              imgExhibit.Width := Min(imgExhibit.Picture.Width, MaxWidth div 2);
              FitImage(imgExhibit, True);
              panelExhibit.Left := ScrollBox1.Width - imgExhibit.Width - 24;
            end;
        end
      else
        begin                                            
          imgExhibit.Picture.Clear;
          imgExhibit.Width := 160;
          imgExhibit.Height := 160;                                                                
          panelExhibit.Caption := 'Exhibit File Missing:' + CRLF + fQSetRef.CurrentQuestion.Exhibit;
          panelExhibit.Left := ScrollBox1.Width - imgExhibit.Width - 24;
        end;
    end
  else
    panelExhibit.Visible := False;
end;


{ SHOW ANSWER CHOICES }

procedure TRunTestForm.ShowAnswerChoices;
var
  uOptionsForm: array[1..8] of string;
  NumOpts: Byte;
begin
  uOptionsForm[1] := fQSetRef.CurrentQuestion.Option1;
  uOptionsForm[2] := fQSetRef.CurrentQuestion.Option2;
  uOptionsForm[3] := fQSetRef.CurrentQuestion.Option3;
  uOptionsForm[4] := fQSetRef.CurrentQuestion.Option4;
  uOptionsForm[5] := fQSetRef.CurrentQuestion.Option5;
  uOptionsForm[6] := fQSetRef.CurrentQuestion.Option6;
  uOptionsForm[7] := fQSetRef.CurrentQuestion.Option7;
  uOptionsForm[8] := fQSetRef.CurrentQuestion.Option8;
                                                      
  // Count how many optional answers
  NumOpts := 0;
  while length(uOptionsForm[NumOpts+1]) > 0 do
    inc(NumOpts);

  // If not randomised, do it now
  if (AnswerOrder[fQSetRef.CurrentIndex, 1] = 0) then
    OrderAnswers(NumOpts);

  ShowItem(rb1, cb1, id1, cbl1, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,1]]);
  ShowItem(rb2, cb2, id2, cbl2, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,2]]);
  ShowItem(rb3, cb3, id3, cbl3, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,3]]);
  ShowItem(rb4, cb4, id4, cbl4, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,4]]);
  ShowItem(rb5, cb5, id5, cbl5, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,5]]);
  ShowItem(rb6, cb6, id6, cbl6, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,6]]);
  ShowItem(rb7, cb7, id7, cbl7, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,7]]);
  ShowItem(rb8, cb8, id8, cbl8, uOptionsForm[AnswerOrder[fQSetRef.CurrentIndex,8]]);
end;


{ SHOW ITEM }

{ All items have been positioned horizontally in the designer. Need to set Y
  position dynamically which is helped by "NextFreeY" which is updated after
  each item has been shown to reflect the next Y-pos for drawing. Depending
  on whether this is a single/multiple answer will either show radio buttons
  or check boxes by setting them visible/hidden as appropriate }

procedure TRunTestForm.ShowItem(refButton: TRadioButton; refBox: TCheckBox; refID: TLabel; refText: TLabel; refValue: string);
begin
  if (length(refValue) > 0) then        // Is there an answer to show ?
    begin
      refText.Top := NextFreeY;
      refText.Caption := refValue;
      refText.Visible := True;
      refID.Top       := refText.Top;
      refID.Visible   := True;
      if (fQSetRef.CurrentQuestion.QuestionType = 0) then // Check single/multiple answer
        begin
          refButton.Top     := refText.Top - 1;  // Show radio buttons
          refButton.Visible := True;
          refBox.Visible    := False;
        end
      else
        begin
          refBox.Top        := refText.Top - 1;  // Show check boxes
          refBox.Visible    := True;
          refButton.Visible := False;
        end;
      SetSize(refText, 64 {= leftmargin + checkbox + identifier});
      NextFreeY := refText.Top + refText.Height + 8;
    end
  else
    begin
      refButton.Visible := False;
      refBox.Visible    := False;
      refText.Visible   := False;
      refID.Visible     := False;
    end;
end;


{ In Delphi setting a label's Autosize=True and WordWap=True respects the
  assigned Width and automatically sets Height to allow all the caption text
  to be displayed. The original TestMaster code relied on this. Unfortunately
  this does not appear to be the case for a Lazarus/FPC TLabel which requires
  the "width to be anchored". Hence the following code to calculate the label
  height given the width which is then applied to non-AutoSize labels }

procedure TRunTestForm.SetSize(aLabel: TLabel; aLeftMargin: integer);
var
  ThisWidth: integer;
begin                                                                           
  aLabel.Left := aLeftMargin;
  ThisWidth := GetWidth(aLeftMargin);
  aLabel.Width := ThisWidth;
  aLabel.Height := CalcHeight(ThisWidth, aLabel);
end;


function TRunTestForm.GetWidth(LeftMargin: integer): integer;
begin
  Result := ScrollBox1.Width - LeftMargin - 24 {= right margin + scrollbar};
  //  If Exhibit is shown, adjust widths around it
  if ( panelExhibit.Visible and (panelExhibit.Left > 16) and (NextFreeY < (panelExhibit.Height + 16)) ) then
    Result := Result - panelExhibit.Width - 12;
end;


{ Code extract from TCustomLabel with minor mods  }

function TRunTestForm.CalcHeight(aWidth: integer; aLabel: TLabel): integer;
var
  ThisDC: HDC;
  ThisRect: TRect;
  ThisText: String;
begin
  ThisText := aLabel.Caption;
  ThisDC := GetDC(aLabel.Parent.Handle);
  try
    ThisRect := Rect(0, 0, aWidth, 10000);
    DrawText(ThisDC, PChar(ThisText), Length(ThisText), ThisRect, DT_CALCRECT or DT_WORDBREAK);
    Result := ThisRect.Bottom - ThisRect.Top;
  finally
    ReleaseDC(aLabel.Parent.Handle, ThisDC);
  end;
end;


{ SHOW CORRECT ANSWER }

procedure TRunTestForm.ShowCorrectAnswer(IsShowing: boolean);
var
  i: integer;
  NewColor: TColor;
  LocalAnswer: Byte;
begin
  Line2.Visible := IsShowing;
  lbReference.Visible := IsShowing;
  if (IsShowing) then
    begin
      NewColor := clRed;
      lbReference.Top := NextFreeY;
      lbReference.Caption := 'Notes: ' + fQSetRef.CurrentQuestion.Remarks;
      SetSize(lbReference, 16);
      NextFreeY := NextFreeY + lbReference.Height + 12;
    end
  else
    NewColor := clWindowText;

  // Highlight correct answers
  LocalAnswer := CheckArray[RealQuestionNumber].Answer;
  for i := 0 to ComponentCount - 1 do
     if (Components[i] is TLabel) then
       if ((LocalAnswer and TLabel(Components[i]).Tag) > 0) then
         TLabel(Components[i]).Font.Color := NewColor
       else
         TLabel(Components[i]).Font.Color := clWindowText;
end;


{ BUTTON PREVIOUS }

procedure TRunTestForm.btnPreviousClick(Sender: TObject);
begin
  CheckAnswer;
  if (CurrentQuestion > 1) then
    SetQuestion(CurrentQuestion - 1);
  if (ListBox1.Visible) then
    ListBox1.ItemIndex := CurrentQuestion; // Highlight current question
end;


{ BUTTON NEXT }

procedure TRunTestForm.btnNextClick(Sender: TObject);
begin
  CheckAnswer;                          // Check current answer before ...
  if (CurrentQuestion < MaxNumber) then // moving on
    SetQuestion(CurrentQuestion + 1);
  if (ListBox1.Visible) then
    ListBox1.ItemIndex := CurrentQuestion; // Highlight current question
end;  


{ BUTTON ANSWER }

{ Show the answer and any comments / remarks regarding the current question  }

procedure TRunTestForm.btnAnswerClick(Sender: TObject);
begin
  if (btnAnswer.Caption = 'Answer') then
    begin
      AnswerFlag := True;
      btnAnswer.Caption := 'Hide';
      OldNext := btnNext.Enabled;
      OldPrev := btnPrevious.Enabled;
      OldEnd := btnEndReview.Enabled;
      OldClose := btnClose.Enabled;
      btnNext.Enabled := False;
      btnPrevious.Enabled := False;
      btnGoto.Enabled := False;
      btnEndReview.Enabled := False;
      btnClose.Enabled := False;
    end
  else
    begin
      AnswerFlag := False;
      btnAnswer.Caption := 'Answer';
      btnNext.Enabled := OldNext;
      btnPrevious.Enabled := OldPrev;
      btnGoto.Enabled := True;
      btnEndReview.Enabled := OldEnd;
      btnClose.Enabled := OldClose;
    end;
  CheckAnswer;
  ShowQuestion;                         // Show or hide answer
end;        


{ BUTTON END TEST / REVIEW }

procedure TRunTestForm.btnEndReviewClick(Sender: TObject);
begin
  if (btnEndReview.Caption = 'End Test') then
    begin
      TestTimer.Enabled := False;          // Disable timer and buttons
      btnEndReview.Caption := 'Review';
      btnAnswer.Enabled     := False;
      btnNext.Enabled     := False;
      btnPrevious.Enabled := False;
      btnGoto.Enabled     := False;
      btnMark.Enabled     := False;
      btnClose.Enabled    := True;
      btnSave.Enabled     := True;      // Allow user to save results
      CheckAnswer;                      // In case 'Next' not pressed
      EvaluateResults(True);
    end
  else
    begin
      btnEndReview.Enabled := False;
      ReviewFlag := True;               // So Question # displayed correctly
      SetQuestion(1);                   // Show first error
    end;
end;    


{ BUTTON MARK }

procedure TRunTestForm.btnMarkClick(Sender: TObject);
begin
  CheckArray[RealQuestionNumber].Marked := not CheckArray[RealQuestionNumber].Marked;
  CheckAnswer;
  ShowQuestion;
  if (ListBox1.Visible) then
    btnGotoClick(nil);
end;


{ GOTO  }

procedure TRunTestForm.btnGotoClick(Sender: TObject);
var
  i: integer;
  CheckMark: string;
begin
  if (ListBox1.Visible) then
    ListBox1.Visible := False
  else
    begin
      ListBox1.Visible := True;         // Show goto number box
      ListBox1.Items.Clear;
      ListBox1.Items.Add('Select:');
      for i := 1 to MaxNumber do
        begin
          if (CheckArray[QuestionOrder[i]].Marked) then
            CheckMark := '**'           // Indicate marked status
          else
            CheckMark := '';
          ListBox1.Items.Add(' '+ IntToStr(i) +' '+ CheckMark);
        end;
        ListBox1.ItemIndex := CurrentQuestion; // Highlight current question
    end;
  CheckAnswer;
  ShowQuestion;                         // Update display
end;


{ LIST BOX CLICK }

procedure TRunTestForm.ListBox1Click(Sender: TObject);
begin
  if (ListBox1.ItemIndex > 0) then
     begin
       CheckAnswer;
       SetQuestion(ListBox1.ItemIndex);
     end;
end;

procedure TRunTestForm.ListBox1DblClick(Sender: TObject);
begin
  if (ListBox1.ItemIndex > 0) then
    begin
      ListBox1.Visible := False;
      CheckAnswer;
      SetQuestion(ListBox1.ItemIndex);
    end;
end;


{ ORDER ANSWERS }

procedure TRunTestForm.OrderAnswers(NumOpts: Byte);
var
  i, NextRandom, temp, NewAnswer: byte;
begin
  for i := 1 to 8 do
    AnswerOrder[fQSetRef.CurrentIndex, i] := i; // Initialise options order array
  if (fQSetRef.CurrentQuestion.Randomise) then
    begin
      Randomize;
      for i := NumOpts downto 2 do      // Assumes always 2 choices or more
        begin
          NextRandom := Random(i) + 1; // Ensure in range 1 to 'i'
          // Swap highest entry with random entry
          temp := AnswerOrder[fQSetRef.CurrentIndex, i];
          AnswerOrder[fQSetRef.CurrentIndex, i] := AnswerOrder[fQSetRef.CurrentIndex, NextRandom];
          AnswerOrder[fQSetRef.CurrentIndex, NextRandom] := temp;
        end;
    end;
  // Calculate new answer based on new order (needed even if not randomised)
  NewAnswer := 0;
  for i := 1 to 8 do
    begin
      // Get bit mask for current order index value
      temp := Power2(AnswerOrder[fQSetRef.CurrentIndex, i] - 1);
      if ( (fQSetRef.CurrentQuestion.Answer and temp) > 0 ) then
        NewAnswer := NewAnswer + Power2(i - 1);
    end;
  CheckArray[RealQuestionNumber].Answer := NewAnswer;
end;


function TRunTestForm.Power2(Exp: integer): Byte;
begin
  Result := 1 shl Exp;
end;


{ CHECK BOX / RADIO BUTTON CLICKS }

procedure TRunTestForm.cbl1Click(Sender: TObject);
begin
  cb1.Checked := not cb1.Checked;       // Toggle states
  rb1.Checked := not rb1.Checked;
end;

procedure TRunTestForm.cbl2Click(Sender: TObject);
begin
  cb2.Checked := not cb2.Checked;
  rb2.Checked := not rb2.Checked;
end;

procedure TRunTestForm.cbl3Click(Sender: TObject);
begin
  cb3.Checked := not cb3.Checked;
  rb3.Checked := not rb3.Checked;
end;

procedure TRunTestForm.cbl4Click(Sender: TObject);
begin
  cb4.Checked := not cb4.Checked;
  rb4.Checked := not rb4.Checked;
end;

procedure TRunTestForm.cbl5Click(Sender: TObject);
begin
  cb5.Checked := not cb5.Checked;
  rb5.Checked := not rb5.Checked;
end;

procedure TRunTestForm.cbl6Click(Sender: TObject);
begin
  cb6.Checked := not cb6.Checked;
  rb6.Checked := not rb6.Checked;
end;

procedure TRunTestForm.cbl7Click(Sender: TObject);
begin
  cb7.Checked := not cb7.Checked;
  rb7.Checked := not rb7.Checked;
end;

procedure TRunTestForm.cbl8Click(Sender: TObject);
begin
  cb8.Checked := not cb8.Checked;
  rb8.Checked := not rb8.Checked;
end;


{ BUTTON HELP }

procedure TRunTestForm.btnHelpClick(Sender: TObject);
begin
  RunHelp('run.htm');
end;


{ BUTTON SAVE }

procedure TRunTestForm.btnSaveClick(Sender: TObject);
begin
  btnSave.Enabled := False;
  if (not fIsElapsedTime) then
    TestTime := (AppTimer * 60) - TestTime;
  ResultsForm.Add(FormatDateTime('dd mmm yyyy', Date) + '  Test: ' + fFilename);
  ResultsForm.Add('     Time: ' + ShowTime(TestTime) + ',  ' + lblResults.Caption);
  ResultsForm.Add('');
end;


{ BUTTON CLOSE }

procedure TRunTestForm.btnCloseClick(Sender: TObject);
begin
  IsClosePressed := True;
  Close;
end;


{ CHECK ANSWER }

{ Before moving off this question, record user's answer and correctness }

procedure TRunTestForm.CheckAnswer;
var
  UserAnswer: Byte;
  i: integer;
begin
  if (ReviewFlag) then Exit;            // No need to check if reviewing

  UserAnswer := 0;
  if (fQSetRef.CurrentQuestion.QuestionType = 0) then
    begin
      for i := 0 to (ComponentCount - 1) do
        if (Components[i] is TRadioButton) then
          if (TRadioButton(Components[i]).Checked) then
            UserAnswer := UserAnswer + TRadioButton(Components[i]).Tag;
    end
  else
    begin
      for i := 0 to (ComponentCount - 1) do
        if (Components[i] is TCheckBox) then
          if (TCheckBox(Components[i]).Checked) then
            UserAnswer := UserAnswer + TCheckBox(Components[i]).Tag;
    end;
  CheckArray[RealQuestionNumber].UserAnswer := UserAnswer;
  CheckArray[RealQuestionNumber].Correct := (UserAnswer = CheckArray[RealQuestionNumber].Answer);
  if (AdminOptions.IsRunningScoreEnabled) then
    EvaluateResults(False);
end;


{  EVALUATE THE RESULTS SO FAR }

{ Checks are done up to 'CurrentQuestion' to cater for only partial completion
  of the test by a user. If whole test completed then 'CurrentQuestion' will
  be equal to 'MaxNumber' }

procedure TRunTestForm.EvaluateResults(CheckReview: Boolean);
var
  i, Correct, NumberViewed: integer;
begin
  Correct := 0;
  NumberViewed := 0;
  for i := 1 to MaxNumber do
    begin
      if (CheckArray[i].Viewed) then
        begin
          Inc(NumberViewed);
          if (CheckArray[i].Correct) then
            Inc(Correct);
        end;
      end;
  lblResults.Caption := Format('%d of %d Correct [%d%%]', [Correct, NumberViewed, Round(Correct / NumberViewed * 100)]);
  ErrorsFlag := (Correct <> NumberViewed); // Carry error status forward

  // Only do following if end of test
  if (CheckReview) then
    begin
      lblResults.Visible := True;        // Always display results at end
      if (ErrorsFlag) then               // If errors made ...
        begin
          btnEndReview.Enabled := True;  // enable review button
          ReorderErrors;                 // only show wrong answers
        end
      else
        btnEndReview.Enabled := False;
    end;
end;


procedure TRunTestForm.ReorderErrors;
var
  i, NewIndex: integer;
begin
  NewIndex := 1;
  for i := 1 to MaxNumber do
    begin
      // If viewed AND wrong then reorder
      if (CheckArray[QuestionOrder[i]].Viewed and (CheckArray[QuestionOrder[i]].Correct = False)) then
        begin
          QuestionOrder[NewIndex] := QuestionOrder[i];
          inc(NewIndex);
        end;
    end;
  MaxNumber := NewIndex - 1;
end;  


{ TEST TIMER }

procedure TRunTestForm.StartTimer;
begin
  if (fIsElapsedTime) then
    TestTime := 0                       // Elapsed timer
  else
    TestTime := AppTimer * 60;          // Countdown timer
  TestTimer.Enabled := True;
end;


procedure TRunTestForm.TestTimerTimer(Sender: TObject);
begin
  if (fIsElapsedTime) then
    begin
      Inc(TestTime);
      lbTime.Caption := 'Elapsed time: ' + ShowTime(TestTime);
    end
  else
    begin
      Dec(TestTime);
      lbTime.Caption := 'Time left: ' + ShowTime(TestTime);
      if (TestTime < 1) then
        begin
          TestTimer.Enabled := False;      // Disable timer
          ShowMessage('Allocated test time expired');
          btnEndReviewClick(nil);
        end;
    end;
end;


{ SHOW TIME }

function TRunTestForm.ShowTime(Seconds: integer): string;
var
  Hour, Min, Sec: word;
begin
  Hour := Seconds div 3600;
  Min  := (Seconds - (Hour*3600)) div 60;
  Sec  := Seconds - (Hour*3600) - (Min*60);
  Result := format('%2.2d:%2.2d:%2.2d', [Hour, Min, Sec]);
end;    


{ RESIZE }

procedure TRunTestForm.FormResize(Sender: TObject);
begin
  { LogoImage is set to align right, with panelLeft set to align Client.
    Want to adjust LogoImage width according to width of window, but need
    to ensure room for Question # and possible (Marked) flag to left (min 320).
    If the space to the right exceeds the logo image width itself though, want
    to restrict it to that width so that Mark/Goto buttons stick to LHS of logo
  }
  LogoImage.Width := Min(LogoImage.Picture.Width, panelTop.Width - 320);
  ShowQuestion;                         // Redraw question info
end;


{ SET FONTS AND HINTS }

{ Although it worked fine in Delphi, I could not get the ParentFont and
  ParentShowHint to work in this module, so did this fudge to keep same
  functionality... STILL DOES NOT WORK!! }

procedure TRunTestForm.SetFontAndHints;
var
  i: integer;
begin
  with TForm(self) do
  begin
    for i := 0 to (ComponentCount - 1) do
    begin
      if (Components[i] is TButton) then
        TButton(Components[i]).ShowHint := AppShowHint;

      if (Components[i] is TLabel) then
        if (TLabel(Components[i]).ParentFont) then
          TLabel(Components[i]).Font.Size  := AppFontSize;
    end;
  end;
end;


end.
