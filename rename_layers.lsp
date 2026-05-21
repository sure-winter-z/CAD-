;; ============================================================================
;; rename_layers.lsp  —  CAD图层重命名为 BuildingGenerator 兼容格式
;; ============================================================================
;;  用法（任选一种）：
;;    1. 拖拽本文件到 AutoCAD 绘图窗口 → 输入 BLD → 回车
;;    2. AutoCAD 命令行输入: (load "D:/桌面/rename_layers.lsp") → 回车 → BLD → 回车
;;    3. 菜单 AP → 加载本文件 → 命令行输入 BLD → 回车
;; ============================================================================

(defun c:BLD (/ *error* doc layers lay name low matched
               wall_kw col_kw door_kw win_kw
               n_renamed n_skipped)

  ;; ---------- 错误处理 ----------
  (defun *error* (msg)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg))
    )
    (princ)
  )

  ;; ---------- 关键词列表 (匹配开头，不区分大小写) ----------
  (setq wall_kw   '("WALL"   "A-WALL"   "S-WALL"   "WALLS"
                    "WALL-LINE" "WALL_FULL"
                    "墙体" "外墙" "内墙" "墙"))
  (setq col_kw    '("COL"   "COLUMN"   "S-COL"   "A-COL"   "柱"))
  (setq door_kw   '("DOOR"  "A-DOOR"   "DOORS"   "M-DOOR" "D-DOOR"
                    "DOOR_FULL" "门"))
  (setq win_kw    '("WINDOW" "A-WINDOW" "WINDOWS" "A-GLAZ" "M-WIND"
                    "WINS" "WINDOW_FULL" "窗"))

  (setq n_renamed 0  n_skipped 0)

  ;; ---------- 获取当前文档所有图层 ----------
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (princ "\n========== 开始检测并重命名图层 ==========")

  ;; ---------- 遍历图层 ----------
  (vlax-for lay layers
    (setq name (vla-get-Name lay))
    (setq low  (strcase name))          ;; 转大写做匹配

    (cond
      ;; ---- 墙体 ----
      ((match-keyword low wall_kw)
       (if (/= name "wall")
         (progn
           (princ (strcat "\n  墙体图层: [" name "] -> [wall]"))
           (vla-put-Name lay "wall")
           (setq n_renamed (1+ n_renamed))
         )
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; ---- 柱子 ----
      ((match-keyword low col_kw)
       (if (/= name "col")
         (progn
           (princ (strcat "\n  柱子图层: [" name "] -> [col]"))
           (vla-put-Name lay "col")
           (setq n_renamed (1+ n_renamed))
         )
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; ---- 门 ----
      ((match-keyword low door_kw)
       (if (/= name "door")
         (progn
           (princ (strcat "\n  门图层:   [" name "] -> [door]"))
           (vla-put-Name lay "door")
           (setq n_renamed (1+ n_renamed))
         )
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; ---- 窗 ----
      ((match-keyword low win_kw)
       (if (/= name "window")
         (progn
           (princ (strcat "\n  窗图层:   [" name "] -> [window]"))
           (vla-put-Name lay "window")
           (setq n_renamed (1+ n_renamed))
         )
         (setq n_skipped (1+ n_skipped))
       )
      )

      ;; ---- 不匹配: 跳过 ----
      (T (setq n_skipped (1+ n_skipped)))
    )
  )

  ;; ---------- 结果汇总 ----------
  (princ (strcat "\n========== 完成: 重命名 " (itoa n_renamed)
                 " 个 / 跳过 " (itoa n_skipped) " 个 =========="))
  (if (= n_renamed 0)
    (alert "没有检测到可识别的图层！\n\n请检查:\n- 图层名是否含 墙/柱/门/窗/wall/col/door/window 等关键词\n- 关键词需在图层名的开头位置")
  )
  (princ)
)

;; ============================================================================
;; 辅助函数: 检查图层名是否以任一关键词开头
;; ============================================================================
(defun match-keyword (name kw-list / found)
  (setq found nil)
  (foreach kw kw-list
    (if (and (not found)
             (>= (strlen name) (strlen kw))
             (= (substr name 1 (strlen kw)) kw))
      (setq found T)
    )
  )
  found
)

;; ============================================================================
;; 加载提示
;; ============================================================================
(princ "\n✓ rename_layers.lsp 已加载 — 输入  BLD  执行图层重命名")
(princ)
