{ ==============================================================================

   TIMER VALUE FORM

    When the user selects "Time remaining" for the timer, this form allows the
    user to select the start time (default 60 minutes)

    Interface:
    - TimeLimit; property sets/gets the selected time to start countdown from


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

unit uTimerValueForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Forms, StdCtrls, ExtCtrls, Spin,
  //
  uCommon, Classes;

type
  
  { TTimerValue }

  TTimerValue = class(TForm)
    Label1: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    Label2: TLabel;
    edTimeLimit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetTimeLimit: integer;
    procedure SetTimeLimit(aValue: integer);
  public
    property TimeLimit: integer read GetTimeLimit write SetTimeLimit;
  end;


implementation

{$R *.lfm}


{ CREATE }

procedure TTimerValue.FormCreate(Sender: TObject);
begin
  SwapButtons(btnCancel, btnOk);
end;


{ SHOW }

procedure TTimerValue.FormShow(Sender: TObject);
begin        
  SwapButtons(btnCancel, btnOk);
  ShowHint := AppShowHint;
  edTimeLimit.SetFocus;
end;


{ GET / SET TIME LIMIT }

function TTimerValue.GetTimeLimit: integer;
begin
  Result := edTimeLimit.Value;
end;


procedure TTimerValue.SetTimeLimit(aValue: integer);
begin
  edTimeLimit.Value := aValue;
end;


end.
