{ ==============================================================================

  JSON CONFIG FILE

    This unit allows simple access to a common config file for all other units
    to utilise as required
    - call TConfigFile.Create; sets global variable for AppCfg
    - call relevant read write routines for Integer/Bool/String
    - call Free; closes config file

    If filename is given as 'APP_FILE' it means it refers to the AppName.cfg
    file and that the config file is located in the same folder as the
    executable. Otherwise it expects a full path name


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

unit uJsonCfgFile;

interface

uses
  Forms, SysUtils, JSONIni, FileUtil,
  //
  uCommon;

type
  TOnCustomSectionEvent = procedure(var Value: string) of object;

  { TConfigFile }

  TConfigFile = class(TObject)
  private
    JSONIniFile: TJSONIniFile;
    //
    fOnCustomSection: TOnCustomSectionEvent;
    procedure CheckIfCustomSection(var aSect: string);
  public
    constructor Create(aFilename: string; CustomSectHandler: TOnCustomSectionEvent = nil);
    destructor Destroy; override;
    //
    function  ValueExists(aSection, aKey: string): boolean;
//    procedure DeleteKey(aSection, aKey: string);
//    procedure DeleteSection(aSection: string);                         
    function  ReadString(aSection, aKey, aDefault: string): string;
    procedure WriteString(aSection, aKey, aValue: string);
    function  ReadInteger(aSection, aKey: string; aDefault: integer): integer;
    procedure WriteInteger(aSection, aKey: string; aValue: integer);
    function  ReadBool(aSection, aKey: string; aDefault: boolean): boolean;
    procedure WriteBool(aSection, aKey: string; aValue: boolean);
    //
    property OnCustomSection: TOnCustomSectionEvent read fOnCustomSection write fOnCustomSection;
  end;

var
  AppCfg: TConfigFile;

const
  CFG_WDW_LEFT   = 'Left';                // Common CFG file items
  CFG_WDW_TOP    = 'Top';
  CFG_WDW_WIDTH  = 'Width';
  CFG_WDW_HEIGHT = 'Height';
  CFG_WDW_VIS    = 'Vis';

  APP_FILE       = 'APP';
  SECT_CUSTOM    = '*custom*';


implementation

{ CREATE }

constructor TConfigFile.Create(aFilename: string; CustomSectHandler: TOnCustomSectionEvent);
var
  ThisFile: string;
begin
  try
    if (aFilename = APP_FILE) then
      ThisFile := ChangeFileExt(Application.ExeName, '.cfg')
    else
      ThisFile := aFilename;
    JSONIniFile := TJSONIniFile.Create(ThisFile);
  except
    on E: Exception do
      MessageWarning('Error', Format('Error with config file "%s"; %s', [ThisFile, E.Message]));
  end;
  JSONIniFile.CacheUpdates := True;     // Keep updates in memory, write once
  fOnCustomSection := CustomSectHandler;
end;


{ DESTROY }

destructor TConfigFile.Destroy;
begin
  JSONIniFile.UpdateFile;
  JSONIniFile.Free;
  inherited;
end;


{ Check if key value exists in INI file section }

function TConfigFile.ValueExists(aSection, aKey: string): boolean;
begin
  CheckIfCustomSection(aSection);
  Result := JSONIniFile.ValueExists(aSection, aKey);
end;


{ BOOLEAN }

function TConfigFile.ReadBool(aSection, aKey: string; aDefault: boolean): boolean;
begin
  CheckIfCustomSection(aSection);
  Result := JSONIniFile.ReadBool(aSection, aKey, aDefault);
end;


procedure TConfigFile.WriteBool(aSection, aKey: string; aValue: boolean);
begin
  CheckIfCustomSection(aSection);
  JSONIniFile.WriteBool(aSection, aKey, aValue);
end;


{ INTEGER }

function TConfigFile.ReadInteger(aSection, aKey: string; aDefault: integer): integer;
begin
  CheckIfCustomSection(aSection);
  Result := JSONIniFile.ReadInteger(aSection, aKey, aDefault);
end;


procedure TConfigFile.WriteInteger(aSection, aKey: string; aValue: integer);
begin
  CheckIfCustomSection(aSection);
  JSONIniFile.WriteInteger(aSection, aKey, aValue);
end;


{ STRING }

function TConfigFile.ReadString(aSection, aKey, aDefault: string): string;
begin
  CheckIfCustomSection(aSection);
  Result := JSONIniFile.ReadString(aSection, aKey, aDefault);
end;


procedure TConfigFile.WriteString(aSection, aKey, aValue: string);
begin
  CheckIfCustomSection(aSection);
  JSONIniFile.WriteString(aSection, aKey, aValue);
end;


{ CHECK IF A CUSTOM SECTION }

procedure TConfigFile.CheckIfCustomSection(var aSect: string);
begin
  if (LeftStr(aSect, 8) = SECT_CUSTOM) then
    begin
      aSect := Copy(aSect, 9, 999);
      if Assigned(fOnCustomSection) then
        fOnCustomSection(aSect);
    end;
end;


end.

