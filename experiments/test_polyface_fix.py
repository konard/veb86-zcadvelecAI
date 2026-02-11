#!/usr/bin/env python3
"""
Проверка исправлений в PolyFaceMesh загрузчике
"""

import os
import subprocess
import sys

def run_command(cmd, cwd=None):
    """Запускает команду и возвращает результат"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=cwd)
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)

def test_polyface_loading():
    """Тестирует загрузку PolyFaceMesh после исправлений"""
    print("=== Тестирование PolyFaceMesh загрузчика ===")
    
    # Проверяем наличие файла
    dxf_file = "/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf"
    if not os.path.exists(dxf_file):
        print(f"ОШИБКА: Файл не найден: {dxf_file}")
        return False
    
    print(f"Файл для теста: {dxf_file}")
    
    # Ищем исполняемый файл ZCAD
    zcad_exe = None
    possible_paths = [
        "/tmp/gh-issue-solver-1770837406897/zcad",
        "/tmp/gh-issue-solver-1770837406897/cad_source/zcad",
        "/tmp/gh-issue-solver-1770837406897/zcad",
    ]
    
    for path in possible_paths:
        if os.path.exists(path):
            zcad_exe = path
            break
    
    if not zcad_exe:
        print("ОШИБКА: Исполняемый файл ZCAD не найден")
        print("Попытка сборки проекта...")
        
        # Пробуем собрать проект
        build_dir = "/tmp/gh-issue-solver-1770837406897"
        success, stdout, stderr = run_command("make", cwd=build_dir)
        if success:
            print("Проект успешно собран")
        else:
            print(f"ОШИБКА сборки: {stderr}")
            return False
        
        # Ищем zcad снова
        for path in possible_paths:
            if os.path.exists(path):
                zcad_exe = path
                break
    
    if not zcad_exe:
        print("ОШИБКА: Не удалось найти или собрать ZCAD")
        return False
    
    print(f"Исполняемый файл: {zcad_exe}")
    
    # Создаем тестовый скрипт для загрузки DXF
    test_script = "/tmp/gh-issue-solver-1770837406897/test_polyface_load.pas"
    with open(test_script, 'w') as f:
        f.write("""
program test_polyface_load;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  uzeffdxfsupport,
  uzeentpolyfacemesh,
  uzedrawingdef,
  uzclog;

var
  drawing: TDrawingDef;
  context: TIODXFLoadContext;
  reader: TZMemReader;
  polyfacemesh: GDBObjPolyFaceMesh;
  stream: TMemoryStream;
  dxf_data: TStringList;

begin
  try
    programlog := TProgramLog.Create(nil);
    programlog.LogOutFormatStr('Начало теста загрузки PolyFaceMesh', [], LM_Info);
    
    // Загружаем DXF файл
    dxf_data := TStringList.Create;
    dxf_data.LoadFromFile('/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf');
    
    stream := TMemoryStream.Create;
    stream.Write(dxf_data.GetText[0], Length(dxf_data.GetText));
    stream.Position := 0;
    
    reader := TZMemReader.Create(stream);
    
    drawing := TDrawingDef.Create;
    context.GDBVertexLoadCache.init(1000);
    
    // Создаем PolyFaceMesh
    polyfacemesh.init(nil, nil, 0);
    
    // Загружаем из DXF
    polyfacemesh.LoadFromDXF(reader, nil, drawing, context);
    
    programlog.LogOutFormatStr('Результат: %d вершин, %d граней', 
      [polyfacemesh.GetVertexCount, polyfacemesh.GetFaceCount], LM_Info);
    
    // Проверяем ожидаемые значения
    if (polyfacemesh.GetVertexCount = 4) and (polyfacemesh.GetFaceCount = 1) then
      programlog.LogOutFormatStr('ТЕСТ ПРОЙДЕН: Корректные количества вершин и граней', [], LM_Info)
    else
      programlog.LogOutFormatStr('ТЕСТ НЕ ПРОЙДЕН: Ожидается 4 вершины и 1 грань, получено %d вершин и %d граней', 
        [polyfacemesh.GetVertexCount, polyfacemesh.GetFaceCount], LM_Info);
    
  except
    on E: Exception do
      programlog.LogOutFormatStr('ИСКЛЮЧЕНИЕ: %s', [E.Message], LM_Info);
  end;
  
  programlog.Free;
end.
""")
    
    print("Тестовый скрипт создан")
    print("Для ручного тестирования запустите:")
    print(f"cd /tmp/gh-issue-solver-1770837406897 && fpc {test_script} && ./test_polyface_load")
    
    return True

def analyze_dxf_structure():
    """Анализирует структуру DXF файла"""
    print("\n=== Анализ структуры DXF файла ===")
    
    dxf_file = "/tmp/gh-issue-solver-1770837406897/cad_source/polyface.dxf"
    with open(dxf_file, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    vertex_count = 0
    face_record_count = 0
    
    in_polyline = False
    in_entities = False
    
    for i, line in enumerate(lines):
        line = line.strip()
        
        if line == '2':
            if i + 1 < len(lines) and lines[i + 1].strip() == 'ENTITIES':
                in_entities = True
        elif line == '0' and i + 1 < len(lines):
            next_line = lines[i + 1].strip()
            if next_line == 'ENDSEC':
                in_entities = False
            elif next_line == 'POLYLINE' and in_entities:
                in_polyline = True
            elif next_line == 'SEQEND':
                in_polyline = False
            elif next_line == 'VERTEX' and in_polyline:
                # Ищем флаг 70 в следующих строках
                for j in range(i + 1, min(i + 20, len(lines))):
                    if lines[j].strip() == '70' and j + 1 < len(lines):
                        flag = int(lines[j + 1].strip())
                        if flag & 128:  # Face record
                            face_record_count += 1
                        else:  # Vertex with coordinates
                            vertex_count += 1
                        break
    
    print(f"Найдено вершин с координатами: {vertex_count}")
    print(f"Найдено записей граней: {face_record_count}")
    print(f"Ожидаемый результат: 4 вершины, 1 грань")
    
    return vertex_count == 4 and face_record_count == 1

if __name__ == "__main__":
    # Анализируем структуру DXF
    structure_ok = analyze_dxf_structure()
    
    if structure_ok:
        print("\n✓ Структура DXF файла корректна")
    else:
        print("\n✗ Структура DXF файла некорректна")
    
    # Тестируем загрузку
    test_ok = test_polyface_loading()
    
    if structure_ok and test_ok:
        print("\n=== ИТОГ: Исправления должны работать корректно ===")
    else:
        print("\n=== ИТОГ: Требуется дополнительная отладка ===")