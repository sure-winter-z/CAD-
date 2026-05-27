#!/usr/bin/env python3
# ============================================================================
# organize_layers.py — 自动整理 CAD 图层 (Python + ezdxf)
# ============================================================================
#  用法:
#    python organize_layers.py <输入文件.dxf>
#
#  注意:
#    - 仅支持 DXF 格式。如果你的文件是 DWG，请先在 AutoCAD 中 SAVEAS → DXF
#    - 脚本会在同目录输出 *_organized.dxf
# ============================================================================

import sys
import os
import ezdxf
from ezdxf.entity import Entity

# ============================================================================
# 可配置关键词 — 修改这里即可适配不同图层命名习惯
# ============================================================================
COL_KEYWORDS   = ['COL', 'COLUMN', 'S-COL', 'A-COL', '柱', '柱子', '结构柱']
DOOR_KEYWORDS  = ['DOOR', 'A-DOOR', 'M-DOOR', 'D-DOOR', '门', '平开门', '推拉门', '卷帘门']
WIN_KEYWORDS   = ['WINDOW', 'A-WINDOW', 'A-GLAZ', 'M-WIND', 'WINS', '窗', '窗户', '天窗', '幕墙']

# 需要处理的问题图层 → 目标图层
SOURCE_LAYERS  = ['PUB_WALL', 'BS-承重墙柱', 'DS-门窗']
TARGET_LAYERS  = ['wall', 'col', 'door', 'window']

# 封闭多段线 & 圆的最大面积 (mm²) — 超过此面积的视为墙体而非柱子
MAX_COL_AREA   = 500_000   # 0.5 m²


# ============================================================================
# 工具函数
# ============================================================================

def match_keyword(text, keywords):
    """检查 text (大写) 是否包含任一关键词"""
    upper = text.upper()
    for kw in keywords:
        if kw.upper() in upper:
            return True
    return False


def ensure_layers(doc):
    """确保目标图层存在"""
    for name in TARGET_LAYERS:
        if name not in doc.layers:
            doc.layers.add(name=name)


def move_entity(entity, new_layer):
    """将实体移到指定图层"""
    try:
        entity.dxf.layer = new_layer
    except Exception:
        pass  # 某些特殊实体可能无法改图层


def is_closed_shape(entity):
    """判断是否为封闭图形 (多段线 / 圆 / 椭圆)"""
    t = entity.dxftype()
    if t == 'CIRCLE':
        return True
    if t == 'ELLIPSE':
        return True
    if t in ('LWPOLYLINE', 'POLYLINE'):
        # flags & 1 = closed
        flags = entity.dxf.flags if entity.dxf.hasattr('flags') else 0
        return bool(flags & 1)
    return False


def get_entity_area(entity):
    """获取封闭图形的面积，非封闭图形返回 -1"""
    t = entity.dxftype()
    if t == 'CIRCLE':
        r = entity.dxf.radius
        return 3.14159265 * r * r
    if t == 'ELLIPSE':
        a = entity.dxf.major_axis_param[0] if hasattr(entity.dxf, 'major_axis_param') else 100
        return 3.14159265 * a * 100  # 近似
    return -1  # 多段线面积计算较复杂，返回 -1 表示不按面积筛


def is_block(entity):
    """判断是否为块参照 (INSERT)"""
    return entity.dxftype() == 'INSERT'


def get_block_name(entity):
    """获取块参照的块名"""
    try:
        return entity.dxf.name.upper()
    except Exception:
        return ''


# ============================================================================
# 处理 PUB_WALL → wall
# ============================================================================
def process_pub_wall(msp, doc):
    count = 0
    if 'PUB_WALL' not in doc.layers:
        print("  [PUB_WALL] 图层不存在，跳过")
        return count

    for e in msp:
        if e.dxf.layer == 'PUB_WALL':
            move_entity(e, 'wall')
            count += 1

    print(f"  [PUB_WALL] → [wall]: {count} 个实体")
    return count


# ============================================================================
# 处理 BS-承重墙柱 → col (柱子) + wall (墙体)
# ============================================================================
def process_bs_layer(msp, doc):
    n_col = 0
    n_wall = 0

    if 'BS-承重墙柱' not in doc.layers:
        print("  [BS-承重墙柱] 图层不存在，跳过")
        return 0, 0

    for e in msp:
        if e.dxf.layer != 'BS-承重墙柱':
            continue

        # 规则1: 块参照 → 按块名判断
        if is_block(e):
            bname = get_block_name(e)
            if match_keyword(bname, COL_KEYWORDS):
                move_entity(e, 'col')
                n_col += 1
            else:
                # 非柱块 → 视为墙体
                move_entity(e, 'wall')
                n_wall += 1

        # 规则2: 封闭多段线 / 圆 / 椭圆 → 视为柱子 (按面积排除超大墙体)
        elif is_closed_shape(e):
            area = get_entity_area(e)
            if 0 < area < MAX_COL_AREA:
                move_entity(e, 'col')
                n_col += 1
            elif area < 0:
                # 多段线面积无法简单计算 → 统一视为柱子
                move_entity(e, 'col')
                n_col += 1
            else:
                # 面积过大 → 墙体
                move_entity(e, 'wall')
                n_wall += 1

        # 规则3: 其余所有线、弧、文字 → 墙体
        else:
            move_entity(e, 'wall')
            n_wall += 1

    print(f"  [BS-承重墙柱] → [col]: {n_col} 个, [wall]: {n_wall} 个")
    return n_col, n_wall


# ============================================================================
# 处理 DS-门窗 → window + door + wall
# ============================================================================
def process_ds_layer(msp, doc):
    n_win = 0
    n_door = 0
    n_wall = 0

    if 'DS-门窗' not in doc.layers:
        print("  [DS-门窗] 图层不存在，跳过")
        return 0, 0, 0

    for e in msp:
        if e.dxf.layer != 'DS-门窗':
            continue

        if is_block(e):
            bname = get_block_name(e)
            if match_keyword(bname, WIN_KEYWORDS):
                move_entity(e, 'window')
                n_win += 1
            elif match_keyword(bname, DOOR_KEYWORDS):
                move_entity(e, 'door')
                n_door += 1
            else:
                move_entity(e, 'wall')
                n_wall += 1
        else:
            # 非块实体 (线段、弧、文字) → 墙体
            move_entity(e, 'wall')
            n_wall += 1

    print(f"  [DS-门窗] → [window]: {n_win}, [door]: {n_door}, [wall]: {n_wall}")
    return n_win, n_door, n_wall


# ============================================================================
# 清理旧图层
# ============================================================================
def cleanup_layers(doc):
    for name in SOURCE_LAYERS:
        if name in doc.layers:
            try:
                doc.layers.remove(name)
                print(f"  已删除图层: [{name}]")
            except ezdxf.DXFError as ex:
                print(f"  无法删除图层 [{name}]: {ex}")


# ============================================================================
# 验证结果
# ============================================================================
def validate(output_path):
    doc = ezdxf.readfile(output_path)
    msp = doc.modelspace()

    layers_present = [layer.dxf.name for layer in doc.layers]
    errors = []

    # 检查必需图层
    for name in TARGET_LAYERS:
        if name not in layers_present:
            errors.append(f"缺少图层: [{name}]")
        else:
            n = len([e for e in msp if e.dxf.layer == name])
            if n == 0:
                errors.append(f"图层 [{name}] 无实体")

    # 检查旧图层是否残留
    for name in SOURCE_LAYERS:
        if name in layers_present:
            errors.append(f"旧图层未删除: [{name}]")

    # 检查是否有多余图层 (除目标图层外的建筑图层)
    allowed = set(TARGET_LAYERS)
    extra = [n for n in layers_present if n not in allowed and n not in ('0', 'Defpoints')]
    # 不强制报错，只提示
    if extra:
        print(f"  提示: 还有其他图层存在: {extra}")

    if errors:
        print("\n========== 失败 ==========")
        for e in errors:
            print(f"  ✗ {e}")
        return False
    else:
        print(f"\n========== 成功: 图层整理完毕 ==========")
        print(f"  最终图层列表: {TARGET_LAYERS}")
        for name in TARGET_LAYERS:
            n = len([e for e in msp if e.dxf.layer == name])
            print(f"    [{name}] : {n} 个实体")
        return True


# ============================================================================
# 主入口
# ============================================================================
def main():
    if len(sys.argv) < 2:
        print("用法: python organize_layers.py <文件.dxf>")
        print("示例: python organize_layers.py D:/桌面/1F.dxf")
        sys.exit(1)

    input_path = sys.argv[1]
    if not os.path.exists(input_path):
        print(f"文件不存在: {input_path}")
        sys.exit(1)

    if not input_path.lower().endswith('.dxf'):
        print("警告: 仅支持 DXF 格式。如果是 DWG 文件，请先在 AutoCAD 中")
        print("      执行命令 SAVEAS → 文件类型选 DXF → 保存")

    # 生成输出路径
    base, ext = os.path.splitext(input_path)
    output_path = base + '_organized' + (ext if ext else '.dxf')

    print(f"输入文件: {input_path}")
    print(f"输出文件: {output_path}")
    print("=" * 50)

    # 读取 & 处理
    doc = ezdxf.readfile(input_path)
    msp = doc.modelspace()

    ensure_layers(doc)

    print("\n[1/3] 处理 PUB_WALL → wall")
    process_pub_wall(msp, doc)

    print("\n[2/3] 处理 BS-承重墙柱 → col + wall")
    process_bs_layer(msp, doc)

    print("\n[3/3] 处理 DS-门窗 → window + door + wall")
    process_ds_layer(msp, doc)

    print("\n清理旧图层...")
    cleanup_layers(doc)

    # 保存 & 验证
    doc.saveas(output_path)
    print(f"\n已保存: {output_path}")

    validate(output_path)


if __name__ == '__main__':
    main()
