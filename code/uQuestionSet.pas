{ ==============================================================================

  QUESTION SET

    This unit provides the functionality to support a Question Set (test)


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

unit uQuestionSet;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uCommon;

type
  TSearchMode = (smFirst, smNext);

  TQuestion = record
    Question: string;
    QuestionType: byte;
    Randomise: boolean;
    Option1: string;
    Option2: string;
    Option3: string;
    Option4: string;
    Option5: string;
    Option6: string;
    Option7: string;
    Option8: string;
    Answer: byte;
    Remarks: string;
    Exhibit: string;
  end;

  TQuestionSet = class(TObject)
  private
    fQSet: array [0..250] of TQuestion;
    fCurrentIndex: integer;
    fMaxInSet: integer;  { Number of questions: 0 = empty (summary only), -1 = no question set loaded  }
    procedure WriteStr(Stream: TFileStream; Value: string);
    procedure ReadStr(Stream: TFileStream; out Value: string);
    function  GetQuestion: TQuestion;
    procedure SetIndex(Value: integer);
    procedure SetLastIndex(Value: integer);
  public
    constructor Create;
    procedure Add(Question: TQuestion);
    procedure Update(Question: TQuestion);
    procedure Delete;
    procedure Clear(aTitle: string);
    function  LoadFromFile(const Filename: string): boolean;
    procedure SaveToFile(const Filename: string);
    function  Search(SearchString: string; Mode: TSearchMode): boolean;
    //
    property  CurrentIndex: integer read fCurrentIndex write SetIndex;
    property  LastIndex: integer read fMaxInSet write SetLastIndex;
    property  CurrentQuestion: TQuestion read GetQuestion;
  end;


implementation

const
  TMQ_VER = 'TMQ_V4';

{ CREATE }

constructor TQuestionSet.Create;
begin
  inherited Create;
  fMaxInSet := -1;                      // No question set loaded
  fCurrentIndex := 0;
end;


{  PROPERTY HELPERS }

function TQuestionSet.GetQuestion: TQuestion;
begin
  Result := fQSet[fCurrentIndex];
end;


procedure TQuestionSet.SetIndex(Value: Integer);
begin
  if ( (Value >= 0) and (Value <= fMaxInSet) ) then
    fCurrentIndex := Value;
end;


procedure TQuestionSet.SetLastIndex(Value: Integer);
begin
  if ( (Value >= 0) and (Value <= 250) ) then
    fMaxInset := Value;
end;


{ ADD QUESTION }

procedure TQuestionSet.Add(Question: TQuestion);
begin
  if (fMaxInSet < 250) then
    begin
      inc(fMaxInSet);
      fQSet[fMaxInSet] := Question;
      fCurrentIndex := fMaxInSet;
    end;
end;


{ UPDATE QUESTION }

procedure TQuestionSet.Update(Question: TQuestion);
begin
  fQSet[fCurrentIndex] := Question;
end;


{ DELETE QUESTION }

procedure TQuestionSet.Delete;
var
  i: Integer;
begin
  // If more than one entry, then copy entries down. If only one then
  // set fMaxInset = 0, i.e. an empty set. Do not permit summary to be deleted
  if (fMaxInSet > 1) then
    begin
      for i := fCurrentIndex to (fMaxInSet - 1) do
        fQSet[i] := fQSet[i+1];
        dec(fMaxInSet);
    end
  else
    if (fMaxInset = 1) then
      fMaxInSet := 0;
  if (fCurrentIndex > fMaxInSet) then
    fCurrentIndex := fMaxInSet;
end;


{ CLEAR QUESTION SET - empty database }

procedure TQuestionSet.Clear(aTitle: string);
begin
  fMaxInSet := 0;
  fCurrentIndex := 0;
  fQSet[0].Question := UpperCase(aTitle) + ' TEST MODULE' + CRLF + CRLF +
                       'Summary of the test module contents, what it relates to and recognition of where the questions came from if they are not original.' + CRLF + CRLF +
                       'An exhibit, if provided, is shown above the summary in place of the default logo. It can be any size but only the top left of the image will be shown ' +
                       'up to 120 pixels high, and the width of the summary section of the window.' + CRLF + CRLF +
                       'Entered: ' + FormatDateTime('DD MMM YYYY', Now);
  fQSet[0].QuestionType := 1;
  fQSet[0].Randomise := True;
  fQSet[0].Option1 := 'If this box is selected, question order will be randomised';
  fQSet[0].Option2 := '';
  fQSet[0].Option3 := '';
  fQSet[0].Option4 := '';
  fQSet[0].Option5 := '';
  fQSet[0].Option6 := '';
  fQSet[0].Option7 := '';
  fQSet[0].Option8 := '';
  fQSet[0].Answer  := 1;
  fQSet[0].Remarks := '';
  fQSet[0].Exhibit := '';
end;


{ SAVE TO FILE }

procedure TQuestionSet.SaveToFile(const Filename: string);
var
  i: integer;
  Stream: TFileStream;
  ThisQuestion: TQuestion;
begin
  if (fMaxInSet >= 0) then               // Do not save if no file loaded (-1)
    begin
      Stream := TFileStream.Create(Filename, fmCreate);
      try
        // Write version identifier
        WriteStr(Stream, TMQ_VER);
        // Write the questions
        Stream.Write(fMaxInSet, sizeof(Integer));
        for i := 0 to fMaxInSet do
          begin
            ThisQuestion := fQSet[i];
            WriteStr(Stream, ThisQuestion.Question);
            Stream.Write(ThisQuestion.QuestionType, 1);
            Stream.Write(ThisQuestion.Randomise, SizeOf(Boolean));
            WriteStr(Stream, ThisQuestion.Option1);
            WriteStr(Stream, ThisQuestion.Option2);
            WriteStr(Stream, ThisQuestion.Option3);
            WriteStr(Stream, ThisQuestion.Option4);
            WriteStr(Stream, ThisQuestion.Option5);
            WriteStr(Stream, ThisQuestion.Option6);
            WriteStr(Stream, ThisQuestion.Option7);
            WriteStr(Stream, ThisQuestion.Option8);
            Stream.Write(ThisQuestion.Answer, 1);
            WriteStr(Stream, ThisQuestion.Remarks);
            WriteStr(Stream, ThisQuestion.Exhibit);
          end;
      finally
        Stream.Free;
      end;
    end;
end;


procedure TQuestionSet.WriteStr(Stream: TFileStream; Value: string);
var
  ThisString: string;
  ThisLength: Integer;
begin
  // ThisString := Encrypt(Value);
  ThisString := Value;
  ThisLength := Length(ThisString);
  Stream.Write(ThisLength, SizeOf(Integer));
  Stream.Write(ThisString[1], ThisLength);
end;


{ LOAD FROM FILE }

function TQuestionSet.LoadFromFile(const Filename: string): boolean;
var
  i: byte;
  Stream: TFileStream;
  Ver: string;
begin
  Result := False;                      // Indicates error in read
  fCurrentIndex := 0;
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    ReadStr(Stream, Ver);
    if (Ver <> TMQ_VER) then Exit;      // Cannot load if incompatible

    Result := True;                     // Version ok
    // Read question set
    Stream.Read(fMaxInSet, sizeof(Integer));
    for i := 0 to fMaxInSet do
      begin
        ReadStr(Stream, fQSet[i].Question);
        Stream.Read(fQSet[i].QuestionType, 1);
        Stream.Read(fQSet[i].Randomise, SizeOf(Boolean));
        ReadStr(Stream, fQSet[i].Option1);
        ReadStr(Stream, fQSet[i].Option2);
        ReadStr(Stream, fQSet[i].Option3);
        ReadStr(Stream, fQSet[i].Option4);
        ReadStr(Stream, fQSet[i].Option5);
        ReadStr(Stream, fQSet[i].Option6);
        ReadStr(Stream, fQSet[i].Option7);
        ReadStr(Stream, fQSet[i].Option8);
        Stream.Read(fQSet[i].Answer, 1);
        ReadStr(Stream, fQSet[i].Remarks);
        ReadStr(Stream, fQSet[i].Exhibit);
     end;
  finally
    Stream.Free;
  end;
end;


procedure TQuestionSet.ReadStr(Stream: TFileStream; out Value: string);
var
  ThisLength: Integer;
begin
  Stream.Read(ThisLength{%H-}, sizeof(Integer)); // Get length of string
  SetLength(Value, ThisLength);
  Stream.Read(Value[1], ThisLength);             // Read the encrypted string
  // Value := Decrypt(Value);
end;


{ SEARCH }

function TQuestionSet.Search(SearchString: string; Mode: TSearchMode): Boolean;
var
  OriginalPos: Integer;
begin
  Result := False;
  if (SearchString = '') then
    MessageInfo('No search text specified !')
  else
    begin
      OriginalPos := fCurrentIndex;
      SearchString := UpperCase(SearchString);
      if (Mode = smFirst) then
        fCurrentIndex := 1
      else
        inc(fCurrentIndex);
        while ( fCurrentIndex <= fMaxInSet ) and ( pos(SearchString,
                       UpperCase(fQSet[fCurrentIndex].Question) +
                       UpperCase(fQSet[fCurrentIndex].Option1) +
                       UpperCase(fQSet[fCurrentIndex].Option2) +
                       UpperCase(fQSet[fCurrentIndex].Option3) +
                       UpperCase(fQSet[fCurrentIndex].Option4) +
                       UpperCase(fQSet[fCurrentIndex].Option5) +
                       UpperCase(fQSet[fCurrentIndex].Option6) +
                       UpperCase(fQSet[fCurrentIndex].Option7) +
                       UpperCase(fQSet[fCurrentIndex].Option8) +
                       UpperCase(fQSet[fCurrentIndex].Remarks) ) = 0 ) do
          inc(fCurrentIndex);
        if (fCurrentIndex > fMaxInSet) then
          begin
            Result := False;
            fCurrentIndex := OriginalPos;
          end
        else
          Result := True;
    end;
end;


end.
