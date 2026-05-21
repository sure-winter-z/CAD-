;; ============================================================================
;; rename_layers.lsp  —  CAD图层重命名为 BuildingGenerator 兼容格式 (v2.1)
;; ============================================================================
;;  用法（任选一种）：
;;    1. 拖拽本文件到 AutoCAD 绘图窗口 → 输入 BLD → 回车
;;    2. AutoCAD 命令行输入: (load "D:/桌面/rename_layers.lsp") → BLD → 回车
;;    3. 菜单 AP → 加载本文件 → 输入 BLD → 回车
;;
;;  v2.1 改进:
;;    - 先列出全部图层名，方便排查
;;    - 改为"包含"匹配，关键词在图层名任意位置都能识别
;;    - 跳过 0 / Defpoints / xref 等系统图层
;;    - 修复重名冲突处理
;;    - 新命令 DIAG：仅列出图层不做修改
;; ============================================================================

;; ---------- 关键词列表 ----------
(setq *wall_kw*
  '("WALL"  "A-WALL"  "S-WALL"  "WALLS"  "WALL-LINE"  "WALL_FULL"
    "A-WALL-FULL"  "S-WALL-FULL"  "WALL-LINE-FULL"  "WALL-LINE-PATT"
    "A-WALL-PATT"  "S-WALL-PATT"
    "墙体"  "外墙"  "内墙"  "墙"  "承重墙"  "隔墙"))

(setq *col_kw*
  '("COL"  "COLUMN"  "S-COL"  "A-COL"  "COLU"  "柱"  "柱子"  "结构柱"))

(setq *door_kw*
  '("DOOR"  "A-DOOR"  "DOORS"  "M-DOOR"  "D-DOOR"
    "DOOR_FULL"  "A-DOOR-FULL"  "DOOR-FULL"
    "门"  "平开门"  "推拉门"  "卷帘门"))

(setq *win_kw*
  '("WINDOW"  "A-WINDOW"  "WINDOWS"  "A-GLAZ"  "M-WIND"
    "WINS"  "WINDOW_FULL"  "A-GLAZ-FULL"  "WINDOW-FULL"
    "窗"  "窗户"  "天窗"  "幕墙"))

;; ============================================================================
;; DIAG 命令 — 仅列出所有图层（不做修改）
;; ============================================================================
(defun c:DIAG (/ doc layers lay name i)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (setq i 0)
  (princ "\n========== 当前 DWG 全部图层 ==========")
  (vlax-for lay layers
    (setq name (vla-get-Name lay))
    (setq i (1+ i))
    (princ (strcat "\n  " (itoa i) ". [" name "]"))
  )
  (princ (strcat "\n========== 共 " (itoa i) " 个图层 =========="))
  (princ)
)

;; ============================================================================
;; BLD 命令 — 检测并重命名图层
;; ============================================================================
(defun c:BLD (/ *error* doc layers lay name low
               n_renamed n_skipped result i)

  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg))
    )
    (princ)
  )

  (setq n_renamed 0  n_skipped 0)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  ;; ---- 第1步：列出全部图层 ----
  (princ "\n========== 当前 DWG 全部图层 ==========")
  (setq i 0)
  (vlax-for lay layers
    (setq name (vla-get-Name lay))
    (setq i (1+ i))
    (princ (strcat "\n  " (itoa i) ". [" name "]"))
  )
  (princ (strcat "\n========== 共 " (itoa i) " 个图层 =========="))

  ;; ---- 第2步：匹配并重命名 ----
  (princ "\n========== 开始匹配重命名 ==========")

  (vlax-for lay layers
    (setq name (vla-get-Name lay))
    (setq low  (strcase name))

    (cond
      ;; 跳过系统图层
      ((or (= low "0")
           (= low "DEFPOINTS")
           (wcmatch low "*|*"))    ;; xref 图层
       (setq n_skipped (1+ n_skipped))
      )

      ;; 墙体
      ((match-kw low *wall_kw*)
       (if (> (do-rename lay "wall" "墙体") 0)
         (setq n_renamed (1+ n_renamed))
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; 柱子
      ((match-kw low *col_kw*)
       (if (> (do-rename lay "col" "柱子") 0)
         (setq n_renamed (1+ n_renamed))
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; 门
      ((match-kw low *door_kw*)
       (if (> (do-rename lay "door" "门") 0)
         (setq n_renamed (1+ n_renamed))
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; 窗
      ((match-kw low *win_kw*)
       (if (> (do-rename lay "window" "窗") 0)
         (setq n_renamed (1+ n_renamed))
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; 不匹配
      (T (setq n_skipped (1+ n_skipped)))
    )
  )

  ;; ---- 第3步：汇总 ----
  (princ (strcat "\n========== 完成: 重命名 " (itoa n_renamed)
                 " 个 / 跳过 " (itoa n_skipped) " 个 =========="))
  (if (= n_renamed 0)
    (alert (strcat
      "没有检测到可识别的图层！\n\n"
      "请向上翻命令行窗口，把图层名发给我，\n"
      "我来添加匹配规则。\n\n"
      "也可以输入 DIAG 命令重新查看图层列表。"))
  )
  (princ)
)

;; ============================================================================
;; 执行重命名（带错误捕获）
;; ============================================================================
(defun do-rename (lay new-name type-label / result)
  (if (= (strcase (vla-get-Name lay)) (strcase new-name))
    (progn
      (princ (strcat "\n  " type-label "图层: [" (vla-get-Name lay) "] 已正确命名，跳过"))
      0
    )
    (progn
      (setq result (vl-catch-all-apply
                     'vla-put-Name
                     (list lay new-name)))
      (if (vl-catch-all-error-p result)
        (progn
          (princ (strcat "\n  !! " type-label "图层改名失败: ["
                         (vla-get-Name lay) "] -> [" new-name "]"
                         "  原因: " (vl-catch-all-error-message result)))
          0
        )
        (progn
          (princ (strcat "\n  " type-label "图层: [" (vla-get-Name lay) "] -> [" new-name "]"))
          1
        )
      )
    )
  )
)

;; ============================================================================
;; 关键词匹配：检查 low 是否包含 kw-list 中任一关键词
;; ============================================================================
(defun match-kw (low kw-list / found)
  (setq found nil)
  (foreach kw kw-list
    (if (and (not found) (vl-string-search kw low))
      (setq found T)
    )
  )
  found
)

;; ============================================================================
;; 加载提示
;; ============================================================================
(princ "\n========================================")
(princ "\n rename_layers.lsp v2.1 已加载")
(princ "\n  BLD  = 列出图层 + 重命名")
(princ "\n  DIAG = 仅列出图层（不修改）")
(princ "\n========================================")
(princ)
