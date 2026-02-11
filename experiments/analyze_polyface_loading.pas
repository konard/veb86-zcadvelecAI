{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Анализатор PolyFaceMesh)
@created(2025)
@desc(Скрипт для анализа загрузки PolyFaceMesh из DXF файла)
}

program analyze_polyface_loading;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  uzclog,
  uzctnrvectorbytes,
  uzctransform,
  uzegeometry,
  uzbtypes,
  uzeentities,
  uzeentity,
  uzeentpolyface,
  uzcdrawings,
  uzcinterface,
  gzctnrVectorTypes;

type
  // Структура для хранения информации о вершине
  TVertexInfo = record
    X, Y, Z: Double;
    Flag: Integer;
    Index: Integer;
  end;

  // Структура для хранения информации о грани
  TFaceInfo = record
    VertexIndices: array[0..3] of Integer;
    Flag: Integer;
    Index: Integer;
  end;

var
  VertexList: array of TVertexInfo;
  FaceList: array of TFaceInfo;
  FilePath: string;
  Lines: TStringList;
  i: Integer;
  CurrentVertex: TVertexInfo;
  CurrentFace: TFaceInfo;
  VertexCount: Integer;
  FaceCount: Integer;

// Процедура для логирования информации о вершине
procedure LogVertexInfo(const AVertex: TVertexInfo);
begin
  programlog.LogOutFormatStr('Вершина %d: X=%.6f, Y=%.6f, Z=%.6f, Flag=%d',
    [AVertex.Index, AVertex.X, AVertex.Y, AVertex.Z, AVertex.Flag], LM_Info);
end;

// Процедура для логирования информации о грани
procedure LogFaceInfo(const AFace: TFaceInfo);
begin
  programlog.LogOutFormatStr('Грань %d: [%d,%d,%d,%d], Flag=%d',
    [AFace.Index, AFace.VertexIndices[0], AFace.VertexIndices[1],
     AFace.VertexIndices[2], AFace.VertexIndices[3], AFace.Flag], LM_Info);
end;

// Функция для проверки флага вершины PolyFaceMesh
function IsPolyFaceVertex(const AFlag: Integer): Boolean;
begin
  // Для вершин PolyFaceMesh флаг обычно равен 192 (0xC0)
  // Бит 6 (64) - полилиния 3D
  // Бит 7 (128) - вершина сетки граней (polyface mesh vertex)
  Result := (AFlag and $C0) = $C0;
end;

// Функция для проверки флага грани PolyFaceMesh
function IsPolyFaceFace(const AFlag: Integer): Boolean;
begin
  // Для граней PolyFaceMesh флаг обычно равен 128 (0x80)
  // Бит 7 (128) - запись грани (face record)
  Result := (AFlag and $80) = $80;
end;

// Процедура для анализа VERTEX записи
procedure AnalyzeVertexEntry(const Lines: TStringList; var StartIndex: Integer);
var
  Line, Code, Value: string;
  Pos: Integer;
  VertexType: string;
  HasCoords: Boolean;
begin
  HasCoords := False;
  CurrentVertex.X := 0.0;
  CurrentVertex.Y := 0.0;
  CurrentVertex.Z := 0.0;
  CurrentVertex.Flag := 0;
  VertexType := '';

  // Анализируем строки до следующей записи или конца
  while StartIndex < Lines.Count do
  begin
    Line := Trim(Lines[StartIndex]);
    
    // Проверяем на начало новой записи
    if (Line = '0') or (Line = 'SEQEND') then
    begin
      Dec(StartIndex);
      Break;
    end;

    if StartIndex + 1 < Lines.Count then
    begin
      Code := Trim(Lines[StartIndex]);
      Value := Trim(Lines[StartIndex + 1]);
      
      // Анализируем групповые коды
      if Code = '100' then
      begin
        if Value = 'AcDbPolyFaceMeshVertex' then
          VertexType := 'PolyFaceVertex'
        else if Value = 'AcDbFaceRecord' then
          VertexType := 'FaceRecord';
      end
      else if Code = '10' then
      begin
        CurrentVertex.X := StrToFloatDef(Value, 0.0);
        HasCoords := True;
      end
      else if Code = '20' then
      begin
        CurrentVertex.Y := StrToFloatDef(Value, 0.0);
        HasCoords := True;
      end
      else if Code = '30' then
      begin
        CurrentVertex.Z := StrToFloatDef(Value, 0.0);
        HasCoords := True;
      end
      else if Code = '70' then
      begin
        CurrentVertex.Flag := StrToIntDef(Value, 0);
      end
      else if Code = '71' then
      begin
        // Индекс первой вершины грани
        CurrentFace.VertexIndices[0] := StrToIntDef(Value, 0);
      end
      else if Code = '72' then
      begin
        // Индекс второй вершины грани
        CurrentFace.VertexIndices[1] := StrToIntDef(Value, 0);
      end
      else if Code = '73' then
      begin
        // Индекс третьей вершины грани
        CurrentFace.VertexIndices[2] := StrToIntDef(Value, 0);
      end
      else if Code = '74' then
      begin
        // Индекс четвертой вершины грани
        CurrentFace.VertexIndices[3] := StrToIntDef(Value, 0);
      end;
      
      Inc(StartIndex, 2);
    end
    else
      Break;
  end;

  // Сохраняем информацию в зависимости от типа
  if VertexType = 'PolyFaceVertex' then
  begin
    Inc(VertexCount);
    CurrentVertex.Index := VertexCount;
    SetLength(VertexList, VertexCount);
    VertexList[VertexCount - 1] := CurrentVertex;
    
    programlog.LogOutFormatStr('Найдена вершина PolyFace: %d', [VertexCount], LM_Info);
    LogVertexInfo(CurrentVertex);
    
    // Проверяем корректность флага
    if not IsPolyFaceVertex(CurrentVertex.Flag) then
      programlog.LogOutFormatStr('ПРЕДУПРЕЖДЕНИЕ: Некорректный флаг вершины: %d (ожидается 192)', 
        [CurrentVertex.Flag], LM_Info);
  end
  else if VertexType = 'FaceRecord' then
  begin
    Inc(FaceCount);
    CurrentFace.Index := FaceCount;
    CurrentFace.Flag := CurrentVertex.Flag; // Используем тот же флаг
    SetLength(FaceList, FaceCount);
    FaceList[FaceCount - 1] := CurrentFace;
    
    programlog.LogOutFormatStr('Найдена грань PolyFace: %d', [FaceCount], LM_Info);
    LogFaceInfo(CurrentFace);
    
    // Проверяем корректность флага
    if not IsPolyFaceFace(CurrentFace.Flag) then
      programlog.LogOutFormatStr('ПРЕДУПРЕЖДЕНИЕ: Некорректный флаг грани: %d (ожидается 128)', 
        [CurrentFace.Flag], LM_Info);
  end;
end;

// Процедура для анализа POLYLINE записи
procedure AnalyzePolyLineEntry(const Lines: TStringList; var StartIndex: Integer);
var
  Line, Code, Value: string;
  MCount, NCount: Integer;
  IsPolyFaceMesh: Boolean;
begin
  MCount := 0;
  NCount := 0;
  IsPolyFaceMesh := False;

  // Анализируем заголовок POLYLINE
  while StartIndex < Lines.Count do
  begin
    Line := Trim(Lines[StartIndex]);
    
    if Line = '0' then
    begin
      Dec(StartIndex);
      Break;
    end;

    if StartIndex + 1 < Lines.Count then
    begin
      Code := Trim(Lines[StartIndex]);
      Value := Trim(Lines[StartIndex + 1]);
      
      if Code = '100' then
      begin
        if Value = 'AcDbPolyFaceMesh' then
          IsPolyFaceMesh := True;
      end
      else if Code = '71' then
      begin
        MCount := StrToIntDef(Value, 0);
        programlog.LogOutFormatStr('Количество вершин в M: %d', [MCount], LM_Info);
      end
      else if Code = '72' then
      begin
        NCount := StrToIntDef(Value, 0);
        programlog.LogOutFormatStr('Количество вершин в N: %d', [NCount], LM_Info);
      end;
      
      Inc(StartIndex, 2);
    end
    else
      Break;
  end;

  if IsPolyFaceMesh then
    programlog.LogOutFormatStr('Обнаружен PolyFaceMesh', [], LM_Info)
  else
    programlog.LogOutFormatStr('ПРЕДУПРЕЖДЕНИЕ: Это не PolyFaceMesh', [], LM_Info);
end;

// Основная процедура анализа DXF файла
procedure AnalyzeDXFFile(const AFilePath: string);
var
  i: Integer;
  Line: string;
  InEntitiesSection: Boolean;
  InPolyLine: Boolean;
begin
  InEntitiesSection := False;
  InPolyLine := False;
  VertexCount := 0;
  FaceCount := 0;
  SetLength(VertexList, 0);
  SetLength(FaceList, 0);

  programlog.LogOutFormatStr('Начало анализа файла: %s', [AFilePath], LM_Info);
  
  Lines.LoadFromFile(AFilePath);
  programlog.LogOutFormatStr('Загружено строк: %d', [Lines.Count], LM_Info);

  i := 0;
  while i < Lines.Count do
  begin
    Line := Trim(Lines[i]);
    
    // Ищем секцию ENTITIES
    if Line = '2' then
    begin
      if (i + 1 < Lines.Count) and (Trim(Lines[i + 1]) = 'ENTITIES') then
      begin
        InEntitiesSection := True;
        programlog.LogOutFormatStr('Вход в секцию ENTITIES', [], LM_Info);
        Inc(i, 2);
        Continue;
      end;
    end
    // Выход из секции ENTITIES
    else if Line = '0' then
    begin
      if (i + 1 < Lines.Count) and (Trim(Lines[i + 1]) = 'ENDSEC') then
      begin
        InEntitiesSection := False;
        programlog.LogOutFormatStr('Выход из секции ENTITIES', [], LM_Info);
        Inc(i, 2);
        Continue;
      end;
      
      // Проверяем на POLYLINE
      if (i + 1 < Lines.Count) and (Trim(Lines[i + 1]) = 'POLYLINE') then
      begin
        if InEntitiesSection then
        begin
          InPolyLine := True;
          programlog.LogOutFormatStr('Найдена запись POLYLINE', [], LM_Info);
          
          // Анализируем заголовок POLYLINE
          Inc(i, 2);
          AnalyzePolyLineEntry(Lines, i);
        end;
      end
      // Проверяем на VERTEX
      else if (i + 1 < Lines.Count) and (Trim(Lines[i + 1]) = 'VERTEX') then
      begin
        if InEntitiesSection and InPolyLine then
        begin
          programlog.LogOutFormatStr('Найдена запись VERTEX', [], LM_Info);
          
          // Анализируем VERTEX запись
          Inc(i, 2);
          AnalyzeVertexEntry(Lines, i);
        end;
      end
      // Проверяем на SEQEND (конец последовательности)
      else if (i + 1 < Lines.Count) and (Trim(Lines[i + 1]) = 'SEQEND') then
      begin
        if InPolyLine then
        begin
          InPolyLine := False;
          programlog.LogOutFormatStr('Завершение последовательности вершин', [], LM_Info);
        end;
      end;
    end;
    
    Inc(i);
  end;
  
  // Выводим итоговую статистику
  programlog.LogOutFormatStr('=== ИТОГОВЫЙ АНАЛИЗ ===', [], LM_Info);
  programlog.LogOutFormatStr('Найдено вершин: %d', [VertexCount], LM_Info);
  programlog.LogOutFormatStr('Найдено граней: %d', [FaceCount], LM_Info);
  
  // Проверяем корректность индексов граней
  for i := 0 to High(FaceList) do
  begin
    if FaceList[i].VertexIndices[0] > VertexCount then
      programlog.LogOutFormatStr('ОШИБКА: Грань %d ссылается на несуществующую вершину %d', 
        [i + 1, FaceList[i].VertexIndices[0]], LM_Info);
    if FaceList[i].VertexIndices[1] > VertexCount then
      programlog.LogOutFormatStr('ОШИБКА: Грань %d ссылается на несуществующую вершину %d', 
        [i + 1, FaceList[i].VertexIndices[1]], LM_Info);
    if FaceList[i].VertexIndices[2] > VertexCount then
      programlog.LogOutFormatStr('ОШИБКА: Грань %d ссылается на несуществующую вершину %d', 
        [i + 1, FaceList[i].VertexIndices[2]], LM_Info);
    if FaceList[i].VertexIndices[3] > VertexCount then
      programlog.LogOutFormatStr('ОШИБКА: Грань %d ссылается на несуществующую вершину %d', 
        [i + 1, FaceList[i].VertexIndices[3]], LM_Info);
  end;
  
  // Выводим координаты всех вершин для проверки
  programlog.LogOutFormatStr('=== КООРДИНАТЫ ВЕРШИН ===', [], LM_Info);
  for i := 0 to High(VertexList) do
  begin
    programlog.LogOutFormatStr('Вершина %d: (%.6f, %.6f, %.6f)', 
      [i + 1, VertexList[i].X, VertexList[i].Y, VertexList[i].Z], LM_Info);
  end;
  
  programlog.LogOutFormatStr('Анализ завершен', [], LM_Info);
end;

begin
  try
    // Инициализируем логирование
    programlog := TProgramLog.Create(nil);
    
    // Устанавливаем путь к файлу
    FilePath := '/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf';
    
    // Проверяем существование файла
    if not FileExists(FilePath) then
    begin
      programlog.LogOutFormatStr('ОШИБКА: Файл не найден: %s', [FilePath], LM_Info);
      Exit;
    end;
    
    Lines := TStringList.Create;
    try
      AnalyzeDXFFile(FilePath);
    finally
      Lines.Free;
    end;
    
  except
    on E: Exception do
    begin
      if Assigned(programlog) then
        programlog.LogOutFormatStr('ИСКЛЮЧЕНИЕ: %s', [E.Message], LM_Info)
      else
        Writeln('ИСКЛЮЧЕНИЕ: ', E.Message);
    end;
  end;
  
  if Assigned(programlog) then
    programlog.Free;
end.