{ ==============================================================================

  SEARCH FORM

    This form supports the find function in the Add/Edit form. It is a
    non-modal form, and questions in the set which contain the search text
    are shown in the Add/Edit window as Next is pressed.

    Interface:
    - QSetRef; property assignment for reference to the main question set
    - OnFind; event assignment for caller's handler when an item is found


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

unit uSearchForm;

{$mode objfpc}{$h+}

interface

uses
  Forms, Dialogs, StdCtrls, Controls, ExtCtrls, Classes,
  //
  uQuestionSet, uCommon;

type
  TOnFindEvent = procedure of object;

  { TSearchForm }

  TSearchForm = class(TForm)
    btnFirst: TButton;
    btnNext: TButton;
    edSearchString: TEdit;
    GroupBox1: TGroupBox;
    lblMessage: TLabel;
    procedure btnFirstClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fOnFind: TOnFindEvent;
    fQSetRef: TQuestionSet;
    procedure DoSearch(Mode: TSearchMode);
  public
    property QSetRef: TQuestionSet read fQSetRef write fQSetRef;
    property OnFind: TOnFindEvent write fOnFind;
  end;


implementation

{$R *.lfm}

{ SHOW }

procedure TSearchForm.FormShow(Sender: TObject);
begin
  ShowHint := AppShowHint;
  lblMessage.Caption := '';
end;


{ BUTTON FIRST }

procedure TSearchForm.btnFirstClick(Sender: TObject);
begin                     
  lblMessage.Caption := '';
  DoSearch(smFirst);
end;


{ BUTTON NEXT }

procedure TSearchForm.btnNextClick(Sender: TObject);
begin
  lblMessage.Caption := '';
  DoSearch(smNext);
end;


{ DO SEARCH }

procedure TSearchForm.DoSearch(Mode: TSearchMode);
begin
  if (edSearchString.Text = '') then
    begin
      lblMessage.Caption := 'No text entered!';
      Exit;
    end;

  if ( Assigned(QSetRef) and Assigned(fOnFind) ) then
    begin
      if (QSetRef.Search(edSearchString.Text, Mode)) then
        fOnFind                         //  Display question
      else
        lblMessage.Caption := 'Text not found';
    end;
end;


end.
