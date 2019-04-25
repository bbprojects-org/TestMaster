{ ==============================================================================

  COMMON TYPES, HELPER FUNCTIONS, ETC

    Provides any type definitions, global info, etc, pertinent to several units


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

unit uCommon;

{$mode objfpc}{$h+}

interface

uses
  {$ifdef darwin}
  baseunix,
  {$endif}
  LCLIntf, Forms, Classes, SysUtils, Dialogs, Controls, StdCtrls, ExtCtrls,
  Zipper, FileUtil, LazFileUtils, LbCipher, LbString, LbUtils;

type
  TAppDataID = (adGlobal, adUser);

const
  {$ifdef darwin}
    DIRECTORY_SEPARATOR = '/';
    CRLF = #10;
  {$else}
    DIRECTORY_SEPARATOR = '\';
    CRLF = #13#10;
  {$endif}

var
  // Global variables
  AppShowHint: boolean;
  AppFontSize: byte;
  AppQFilename: string;
  AppPassword: string;
  AppTimer: integer;
  AppTempDirectory: string;

  // Global routines
  function  GetAppDirectory: string;
  function  GetAppTestsDirectory: string;    
  function  GetAppDataDirectory(id: TAppDataID): string;
  function  GetAppTempDirectory: string;
  function  MessageQuery(aCaption, aText: string): boolean;
  procedure MessageWarning(aCaption, aText: string);
  procedure MessageInfo(aText: string);
  procedure RunHelp(HelpFile: string);
  function  Scramble(WordToScramble: string): string;
  function  UnScramble(WordToUnScramble: string): string;
  procedure SwapButtons({%H-}aButt1, {%H-}aButt2: TButton);
  function  LoadImage(var Image: TImage; Filename: string): Boolean;
  procedure FitImage(aImage:  TImage; Resize: boolean = False);   
  function  UnzipQFiles(aFilename: string): boolean;
  function  ZipQFiles(aFilename: string): boolean;


implementation

{ GET FOLDER / DIRECTORY PATHS }

{ Apple advise should not write to Application bundle as might not have
  permission to do so. Can read Application resources from the bundle though.
  Used for Help files and logo }

function GetAppDirectory: string;
begin
  {$ifdef darwin}
    Result := ExtractFileDir(ExtractFileDir(Application.ExeName)) + '/Resources/';
  {$endif}
  {$ifdef windows}
    Result := ExtractFilePath(Application.ExeName)
  {$endif}
end;


function GetAppTestsDirectory: string;
begin
  Result := GetAppDataDirectory(adGlobal) + 'Tests' + DIRECTORY_SEPARATOR;
end;


{ Wanted to put global app data in "/Library/Application Support" as advised by
  some Apple documentation, but no write privileges in High Sierra so used
  user's Application Support folder }

function GetAppDataDirectory(id: TAppDataID): string;
var
  AppName: string;
begin
  AppName := ExtractFilename(Application.ExeName);
  Result := '/usr/share/' + AppName + '/';
  {$ifdef darwin}
    if (id = adUser) then
      Result := ExpandFileName('~/Library/Application Support/') + AppName + '/'
    else
      // adGlobal
      Result := ExpandFileName('~/Library/Application Support/') + AppName + '/';
  {$endif}
  {$ifdef windows}
    Result := ExtractFilePath(Application.ExeName);
    // Or perhaps...
    // Result := GetEnvironmentVariableUTF8('appdata')+ '\' + AppName;
  {$endif}
end;


{ Create a temporary directory for unzipping the contents of a TMQZ file. It
  simply increments a folder name until the folder does not exist, and uses that }

function GetAppTempDirectory: string;
var
  i: integer;
  prefix: string;
begin
  i := 0;
  prefix := GetAppDataDirectory(adUser) + 'tmp';
  repeat
    Result := Format('%s%.5d', [prefix, i]);
    Inc(i);
  until (not DirectoryExists(Result));
  Result := Result + DIRECTORY_SEPARATOR;
  ForceDirectories(Result);
end;


{ COMMON MESSAGE HELPERS }

function MessageQuery(aCaption, aText: string): boolean;
begin
  Result := MessageDlg(aCaption, aText, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;


procedure MessageWarning(aCaption, aText: string);
begin
  MessageDlg(aCaption, aText, mtWarning, [mbOK], 0);;
end;


procedure MessageInfo(aText: string);
begin
  MessageDlg(aText, mtInformation, [mbOK], 0);;
end;


{ LOAD IMAGE }

function LoadImage(var Image: TImage; Filename: string): Boolean;
begin
  if FileExists(Filename) then
    begin
      Image.Picture.LoadFromFile(Filename);
      Result := True;
    end
  else
    Result := False;
end;


{ RUN HELP }

procedure RunHelp(HelpFile: string);
var
  Filename: string;
begin
  Filename := GetAppDirectory + 'help' + DIRECTORY_SEPARATOR + HelpFile;
  if FileExists(Filename) then
    begin
      OpenDocument(Filename);
    end
  else
    MessageWarning('Show Help', 'Cannot find help file: "' + Filename + '"');
end;


{ ENCRYPT / DECRYPT STRINGS }

var
  Key64: TKey64;

function Scramble(WordToScramble: string): string;
var
  Index: integer;
  PlainText, CipherText: UTF8String;
begin
  for Index := 1 to length(WordToScramble) do
    WordToScramble[Index] := chr( ord(WordToScramble[Index]) + Index);
  Result := WordToScramble;
(*
  RefreshKeys;
  PlainText := StringToUTF8(WordToScramble);
  CipherText := DESEncryptStringEx(PlainText, Key64, True);
  Result := UTF8ToString(CipherText);
*)
end;


function UnScramble(WordToUnScramble: string): string;
var
  Index: integer;          
  PlainText, CipherText : UTF8String;
begin
  for Index := 1 to length(WordToUnScramble) do
    WordToUnScramble[Index] := chr( ord(WordToUnScramble[Index]) - Index);
  Result := WordToUnScramble;
(*
  RefreshKeys;
  CipherText := StringToUTF8(WordToUnScramble);
  PlainText := DESEncryptStringEx(CipherText, Key64, False);
  Result := UTF8ToString(PlainText);
*)
end;

(*
procedure RefreshKeys;
var
  Passphrase: UTF8String;
begin
  Passphrase := StringToUTF8('MyPassword');
  GenerateLMDKey(Key64, SizeOf(Key64), Passphrase);
end;
*)


{ SWAP BUTTONS - assumes button order set ok for MacOS, this will reverse them
                 for the common look on Windows }

procedure SwapButtons(aButt1, aButt2: TButton);
{$ifdef windows}
var
  tmpLeft: integer;
{$endif}
begin
  {$ifdef windows}
    tmpLeft := aButt1.Left;
    aButt2.Left := aButt1.Left;
    aButt1.Left := tmpLeft;
  {$endif}
end;


{ FIT IMAGE }

procedure FitImage(aImage: TImage; Resize: boolean);
begin
  if (aImage.Picture.Width > aImage.Width) or (aImage.Picture.Height > aImage.Height) then
    begin
      aImage.Stretch := True;
      aImage.Proportional := True;
      if (Resize) then
        aImage.Height := trunc((aImage.Picture.Height * aImage.Width) / aImage.Picture.Width);
    end
  else
    begin
      aImage.Stretch := False;
      aImage.Proportional := False;
      if (Resize) then
        begin
          aImage.Width := aImage.Picture.Width;
          aImage.Height := aImage.Picture.Height;
        end;
    end;
end;           


{ UNZIP Q FILES }

{ Unzip all files in the TMQZ archive into the temporary directory, returning
  True if all ok, or False if there are any issues opening the ZIP file }

{ It appears that whilst TUnzipper will Unzip Mac created Zip files with
  correct permissions, it cannot do that with its own Zipped files, instead
  leaving extracted files with No Permissions! Hence Mac specific extra code
  below to set permissions to RW all }

function UnzipQFiles(aFilename: string): boolean;
var
  ThisZip: TUnzipper;
  i: integer;
  ThisFilename: string;
begin
  Result := True;
  ThisZip := TUnzipper.Create;
  try
    ThisZip.FileName := aFilename;
    ThisZip.OutputPath := AppTempDirectory;
    ThisZip.Examine;
    try
      ThisZip.UnZipAllFiles;
      {$ifdef darwin}
      for i := 0 to (ThisZip.Entries.Count - 1) do
        begin
          ThisFilename := AppTempDirectory + ThisZip.Entries.Entries[i].ArchiveFileName;
          fpChmod(ThisFilename, &777);
        end;
      {$endif}
    except
      on E:Exception do
        Result := False;
    end;
  finally
    ThisZip.Free;
  end;
end;


{ ZIP Q FILES }

{ Zip all the files in the temporary directory with relative path names,
  returning True if all ok, or False if there are any issues in doing so

  Relative paths code from:
  https://forum.lazarus.freepascal.org/index.php?topic=39634.0 }

function ZipQFiles(aFilename: string): boolean;
var
  ThisZip: TZipper;
  ThisFileList: TStringList;
  i: integer;
  RelativePath: string;
begin
  Result := True;
  ThisZip := TZipper.Create;
  try
    ThisZip.FileName := aFilename;
    ThisFileList := TStringList.Create;
    try
      FindAllFiles(ThisFileList, AppTempDirectory);
      for i := 0 to (ThisFileList.Count - 1) do
        begin
          RelativePath := CreateRelativePath(ThisFileList[i], AppTempDirectory);
          ThisZip.Entries.AddFileEntry(ThisFileList[i], RelativePath);
        end;
    finally
      ThisFileList.Free;
    end;
    try
      ThisZip.SaveToFile(aFilename);
    except
      on E:Exception do
        Result := False;
    end;
  finally
    ThisZip.Free;
  end;
end;


end.

