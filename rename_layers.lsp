;; ============================================================================
;; rename_layers.lsp  —  CAD图层合并为 BuildingGenerator 兼容格式 (v3.0)
;; ============================================================================
;;  用法：
;;    命令行输入 (load "D:/桌面/rename_layers.lsp") → BLD → 回车
;;    或者 AP 加载 → BLD
;;
;;  v3.0: 相同类型的多个图层会合并到目标层再删掉旧层，解决重名冲突
;; ============================================================================

;; ---------- 关键词列表 ----------
(setq *wall_kw*
  '("WALL"  "A-WALL"  "S-WALL"  "WALLS"  "WALL-LINE"  "WALL_FULL"
    "PUB_WALL"  "A-WALL-FULL"  "S-WALL-FULL"  "WALL-LINE-FULL"
    "A-WALL-PATT"  "S-WALL-PATT"  "WALL-LINE-PATT"
    "墙体"  "外墙"  "内墙"  "墙"  "承重墙"  "非承重墙"  "隔墙"))

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
;; DIAG — 仅列出图层
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
;; BLD — 扫描图层、合并到目标层
;; ============================================================================
(defun c:BLD (/ *error* doc layers lay name low
               n_merged n_skipped n_already)

  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg))
    )
    (princ)
  )

  (setq n_merged 0  n_skipped 0  n_already 0)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  ;; ---- 列出全部图层 ----
  (princ "\n========== 全部图层 ==========")
  (vlax-for lay layers
    (princ (strcat "\n  [" (vla-get-Name lay) "]"))
  )

  ;; ---- 匹配并合并 ----
  (princ "\n\n========== 匹配并合并图层 ==========")

  (vlax-for lay layers
    (setq name (vla-get-Name lay))
    (setq low  (strcase name))

    (cond
      ;; 跳过系统图层
      ((or (= low "0")
           (= low "DEFPOINTS")
           (wcmatch low "*|*"))
       (setq n_skipped (1+ n_skipped)))

      ;; 已经是目标名，跳过
      ((or (= low "WALL") (= low "COL") (= low "DOOR") (= low "WINDOW"))
       (setq n_already (1+ n_already)))

      ;; 墙体
      ((match-kw low *wall_kw*)
       (if (merge-to lay "wall" "墙体")
         (setq n_merged (1+ n_merged))
         (setq n_skipped (1+ n_skipped))))

      ;; 柱子
      ((match-kw low *col_kw*)
       (if (merge-to lay "col" "柱子")
         (setq n_merged (1+ n_merged))
         (setq n_skipped (1+ n_skipped))))

      ;; 门
      ((match-kw low *door_kw*)
       (if (merge-to lay "door" "门")
         (setq n_merged (1+ n_merged))
         (setq n_skipped (1+ n_skipped))))

      ;; 窗
      ((match-kw low *win_kw*)
       (if (merge-to lay "window" "窗")
         (setq n_merged (1+ n_merged))
         (setq n_skipped (1+ n_skipped))))

      ;; 不匹配
      (T (setq n_skipped (1+ n_skipped)))
    )
  )

  ;; ---- 汇总 ----
  (princ (strcat "\n========== 完成 =========="
                 "\n  合并: " (itoa n_merged) " 个"
                 "\n  已是目标: " (itoa n_already) " 个"
                 "\n  跳过: " (itoa n_skipped) " 个"))
  (princ)
)

;; ============================================================================
;; 合并图层：把 lay 中所有实体移到目标层，再删掉 lay
;; ============================================================================
(defun merge-to (lay target-name type-label / ss result n)
  (setq n 0)
  ;; 确保目标层存在
  (ensure-layer target-name)

  ;; 选择 lay 上所有实体
  (setq ss (ssget "X" (list (cons 8 (vla-get-Name lay)))))
  (if ss
    (progn
      ;; 移到目标层
      (setq result (vl-catch-all-apply
                     'vl-cmdf
                     (list "_.CHPROP" ss "" "_LA" target-name "")))
      (if (vl-catch-all-error-p result)
        (princ (strcat "\n  移动实体失败: " (vl-catch-all-error-message result)))
      )
      (setq n (sslength ss))
    )
  )

  ;; 删掉旧图层（不能删当前层、0、Defpoints）
  (if (not (or (= (strcase (vla-get-Name lay)) "0")
               (= (strcase (vla-get-Name lay)) "DEFPOINTS")))
    (vl-catch-all-apply 'vla-delete (list lay))
  )

  (if (> n 0)
    (princ (strcat "\n  " type-label ": [" (vla-get-Name lay) "] → ["
                   target-name "]  (" (itoa n) " 实体)"))
    (princ (strcat "\n  " type-label ": [" (vla-get-Name lay) "] → ["
                   target-name "]  (无实体)"))
  )
  1  ;; 返回成功
)

;; ============================================================================
;; 确保目标图层存在
;; ============================================================================
(defun ensure-layer (name / layers)
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vl-catch-all-apply 'vla-add (list layers name))
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
(princ "\n rename_layers.lsp v3.0 已加载")
(princ "\n  BLD  = 合并图层 (同名类型 → wall/col/door/window)")
(princ "\n  DIAG = 仅列出图层")
(princ "\n========================================")
(princ)
