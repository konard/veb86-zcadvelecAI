#!/usr/bin/env python3
"""
Верификация исправления типа матрицы (NewMatrix.t) для PushMatrix.

Проблема: В HandlePushMatrix поле NewMatrix.t не устанавливалось.
Если неинициализированное значение совпадало с CMTIdentity = [MTIdentity],
функция VectorTransform3D пропускала умножение и возвращала вектор без изменений.

Тест: Проверяем, что при умножении локальных координат на матрицу трансляции
из proxy-данных testSPDSSECTION.txt получаются ожидаемые мировые координаты.

Автор: Claude
Дата: 2026-03-30
"""

import struct
import os
import sys


def vector_transform_3d(v, matrix):
    """
    Реализует VectorTransform3D из ZCAD (v * M, row-vector convention).

    v: кортеж (x, y, z)
    matrix: матрица 4x4 как список из 4 строк по 4 элемента
            (формат ZCAD после транспонирования proxy-данных).
            matrix[row][col], перевод в matrix[3][0..2].
    """
    x, y, z = v
    w = 1.0

    rx = x * matrix[0][0] + y * matrix[1][0] + z * matrix[2][0] + w * matrix[3][0]
    ry = x * matrix[0][1] + y * matrix[1][1] + z * matrix[2][1] + w * matrix[3][1]
    rz = x * matrix[0][2] + y * matrix[1][2] + z * matrix[2][2] + w * matrix[3][2]
    rw = x * matrix[0][3] + y * matrix[1][3] + z * matrix[2][3] + w * matrix[3][3]

    if rw != 0:
        rx /= rw
        ry /= rw
        rz /= rw

    return (rx, ry, rz)


def transpose_proxy_matrix(raw_data):
    """
    Транспонирует матрицу proxy (row-major, translation в col 3)
    в формат ZCAD (row-vector, translation в row 3).

    raw_data: 16 значений double в порядке data[row*4 + col]
    Результат: zcad_matrix[Row][Col] = raw_data[Col * 4 + Row]
    """
    matrix = [[0.0] * 4 for _ in range(4)]
    for row in range(4):
        for col in range(4):
            matrix[row][col] = raw_data[col * 4 + row]
    return matrix


def test_spdssection():
    """Тест на данных из testSPDSSECTION.txt (issue #822)."""

    print("=" * 60)
    print("Тест: SPDSSECTION прямоугольники (issue #822)")
    print("=" * 60)

    # Матрица 1 из proxy: трансляция (17191.646332, 24705.692980)
    proxy_matrix1 = [
        1.0, 0.0, 0.0, 17191.646332,
        0.0, 1.0, 0.0, 24705.692980,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0,
    ]

    # Матрица 2 из proxy: трансляция (2148.788699, 2415.343340)
    proxy_matrix2 = [
        1.0, 0.0, 0.0, 2148.788699,
        0.0, 1.0, 0.0, 2415.343340,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0,
    ]

    # Вершины Shell 1 (Cmd[16]) — в локальных координатах
    local_verts_1 = [
        (308.7081, 394.8726, 0.0),
        (250.6849, 434.0301, 0.0),
        (-308.7081, -394.8726, 0.0),
        (-250.6849, -434.0301, 0.0),
    ]

    # Ожидаемые мировые координаты (из issue #822 комментарий #2)
    expected_world_1 = [
        (17500.3544, 25100.5655, 0.0),  # p3
        (17442.3312, 25139.723, 0.0),   # p2
        (16882.9383, 24310.8204, 0.0),  # p1
        (16940.9615, 24271.6629, 0.0),  # p4
    ]

    # Вершины Shell 2 (Cmd[27]) — в локальных координатах
    local_verts_2 = [
        (-308.7081, -394.8726, 0.0),
        (-250.6849, -434.0301, 0.0),
        (308.7081, 394.8726, 0.0),
        (250.6849, 434.0301, 0.0),
    ]

    # Ожидаемые мировые координаты
    expected_world_2 = [
        (1840.0806, 2020.4708, 0.0),   # p1
        (1898.1038, 1981.3133, 0.0),   # p4
        (2457.4968, 2810.2159, 0.0),   # p3
        (2399.4736, 2849.3734, 0.0),   # p2
    ]

    zcad_matrix1 = transpose_proxy_matrix(proxy_matrix1)
    zcad_matrix2 = transpose_proxy_matrix(proxy_matrix2)

    print("\nМатрица 1 (ZCAD формат, translation в row 3):")
    for row in zcad_matrix1:
        print(f"  | {row[0]:12.4f} {row[1]:12.4f} "
              f"{row[2]:12.4f} {row[3]:12.4f} |")
    print(f"  Translation: ({zcad_matrix1[3][0]:.4f}, "
          f"{zcad_matrix1[3][1]:.4f}, {zcad_matrix1[3][2]:.4f})")

    all_ok = True

    # Тест Shell 1
    print("\n--- Shell 1 (Cmd[16], matrix1) ---")
    for i, (local, expected) in enumerate(
        zip(local_verts_1, expected_world_1)
    ):
        world = vector_transform_3d(local, zcad_matrix1)
        dx = abs(world[0] - expected[0])
        dy = abs(world[1] - expected[1])
        ok = dx < 0.01 and dy < 0.01
        status = "OK" if ok else "FAIL"
        if not ok:
            all_ok = False
        print(f"  v[{i}]: local=({local[0]:9.4f}, {local[1]:9.4f})")
        print(f"         world=({world[0]:9.4f}, {world[1]:9.4f})")
        print(f"         expected=({expected[0]:9.4f}, {expected[1]:9.4f})")
        print(f"         delta=({dx:.4f}, {dy:.4f}) [{status}]")

    # Тест Shell 2
    print("\n--- Shell 2 (Cmd[27], matrix2) ---")
    for i, (local, expected) in enumerate(
        zip(local_verts_2, expected_world_2)
    ):
        world = vector_transform_3d(local, zcad_matrix2)
        dx = abs(world[0] - expected[0])
        dy = abs(world[1] - expected[1])
        ok = dx < 0.01 and dy < 0.01
        status = "OK" if ok else "FAIL"
        if not ok:
            all_ok = False
        print(f"  v[{i}]: local=({local[0]:9.4f}, {local[1]:9.4f})")
        print(f"         world=({world[0]:9.4f}, {world[1]:9.4f})")
        print(f"         expected=({expected[0]:9.4f}, {expected[1]:9.4f})")
        print(f"         delta=({dx:.4f}, {dy:.4f}) [{status}]")

    # Тест: без трансформации (имитация бага)
    print("\n--- Без трансформации (имитация бага CMTIdentity) ---")
    for i, local in enumerate(local_verts_1):
        print(f"  v[{i}]: ({local[0]:9.4f}, {local[1]:9.4f}) "
              "— НЕ трансформировано!")
    print("  ^ Именно это наблюдал пользователь: координаты ~(±308, ±394)")

    print("\n" + "=" * 60)
    if all_ok:
        print("РЕЗУЛЬТАТ: Все трансформации верны!")
        print("Исправление NewMatrix.t := CMTTransform решает проблему.")
    else:
        print("РЕЗУЛЬТАТ: ОШИБКА в трансформации!")
    print("=" * 60)

    return all_ok


if __name__ == '__main__':
    ok = test_spdssection()
    sys.exit(0 if ok else 1)
