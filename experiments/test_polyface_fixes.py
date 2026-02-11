#!/usr/bin/env python3
"""
Тестирование исправлений в PolyFaceMesh
"""
import subprocess
import sys
import os

def run_test():
    print("=== Тестирование исправлений PolyFaceMesh ===")
    
    # Сначала проверяем структуру DXF
    print("1. Проверяем структуру DXF файла...")
    result = subprocess.run([
        'python3', '/tmp/gh-issue-solver-1770837406897/experiments/simple_dxf_analyzer.py'
    ], capture_output=True, text=True, cwd='/tmp/gh-issue-solver-1770837406897')
    
    if result.returncode == 0:
        output = result.stdout
        if "Вершин с координатами: 4" in output and "Записей граней: 1" in output:
            print("✓ Структура DXF корректна")
        else:
            print("✗ Структура DXF некорректна:")
            print(output)
            return False
    else:
        print(f"✗ Ошибка анализатора: {result.stderr}")
        return False
    
    # Проверяем исходный код на наличие исправлений
    print("\n2. Проверяем исходный код...")
    
    source_file = '/tmp/gh-issue-solver-1770837406897/cad_source/zengine/core/entities/uzeentpolyfacemesh.pas'
    with open(source_file, 'r') as f:
        source_code = f.read()
    
    # Проверяем ключевые исправления
    checks = [
        ('context.GDBVertexLoadCache.Clear', 'Очистка кэша в начале'),
        ('currentVertex := NulVertex', 'Сброс координат для новой вершины'),
        ('isPolyFaceVertex', 'Отслеживание типа вершины'),
        ('AcDbPolyFaceMeshVertex', 'Проверка подтипа PolyFaceVertex'),
        ('AcDbFaceRecord', 'Проверка подтипа FaceRecord'),
        ('dxfLoadGroupCodeVertex(rdr,30', 'Добавление вершины при Z-координате')
    ]
    
    all_good = True
    for check, description in checks:
        if check in source_code:
            print(f"✓ {description}")
        else:
            print(f"✗ {description} - НЕ НАЙДЕНО")
            all_good = False
    
    if not all_good:
        print("\n✗ Не все исправления внесены в код")
        return False
    
    print("\n✓ Все исправления внесены в код")
    
    # Пробуем собрать проект
    print("\n3. Проверяем сборку проекта...")
    build_result = subprocess.run([
        'make'
    ], capture_output=True, text=True, cwd='/tmp/gh-issue-solver-1770837406897')
    
    if build_result.returncode == 0:
        print("✓ Проект успешно собран")
    else:
        print(f"✗ Ошибка сборки: {build_result.stderr}")
        return False
    
    print("\n=== ИТОГ: Все проверки пройдены ===")
    print("Исправления должны решить проблему загрузки PolyFaceMesh:")
    print("- Правильное определение типа VERTEX (PolyFaceVertex vs FaceRecord)")
    print("- Предотвращение добавления FaceRecord как вершины с координатами (0,0,0)")
    print("- Очистка кэша от остаточных данных")
    print("- Добавление вершины только после прочтения всех координат")
    
    return True

if __name__ == "__main__":
    success = run_test()
    sys.exit(0 if success else 1)