;; ============================================================================
;; split_layers.lsp  —  拆分混合图层 (v1.0)
;; ============================================================================
;;  用法: (load "D:/桌面/split_layers.lsp")
;;  命令:
;;    SPLIT1  — 拆分 BS-承重墙柱 → 选柱归 col，其余归 wall
;;    SPLIT2  — 拆分 DS-门窗     → 选窗归 window，其余归 door
;;    BLD     — 拆分完成后运行，合并剩余图层
;; ============================================================================

;; ============================================================================
;; SPLIT1: 拆分 BS-承重墙柱
;; ============================================================================
(defun c:SPLIT1 (/ src ss-col ss-wall col-ents n-col n-wall)
  ;; 确保 col 层存在
  (ensure-layer "col")
  (ensure-layer "wall")

  ;; 检查 BS-承重墙柱 图层是否存在
  (if (not (layer-exists "BS-承重墙柱"))
    (progn
      (alert "找不到图层 [BS-承重墙柱]！\n请确认图层名是否正确。")
      (princ)
    )
  )

  (princ "\n========== 拆分 BS-承重墙柱 ==========")
  (princ "\n  请手动框选图层 [BS-承重墙柱] 中的【柱子】→ 回车")

  ;; 选取柱子
  (setq ss-col (ssget '((8 . "BS-承重墙柱"))))
  (if (not ss-col)
    (princ "\n  未选中任何柱子，跳过")
    (progn
      (setq n-col (sslength ss-col))
      ;; 移到 col 层
      (vl-catch-all-apply 'vl-cmdf (list "_.CHPROP" ss-col "" "_LA" "col" ""))
      (princ (strcat "\n  " (itoa n-col) " 个柱子 → [col] 层"))
    )
  )

  ;; 把该层剩余实体移到 wall 层
  (setq ss-wall (ssget "X" '((8 . "BS-承重墙柱"))))
  (if ss-wall
    (progn
      (setq n-wall (sslength ss-wall))
      (vl-catch-all-apply 'vl-cmdf (list "_.CHPROP" ss-wall "" "_LA" "wall" ""))
      (princ (strcat "\n  剩余 " (itoa n-wall) " 个 → [wall] 层"))
    )
  )

  ;; 清理空图层
  (delete-empty-layer "BS-承重墙柱")

  (princ "\n  SPLIT1 完成！")
  (princ)
)

;; ============================================================================
;; SPLIT2: 拆分 DS-门窗
;; ============================================================================
(defun c:SPLIT2 (/ src ss-win ss-door n-win n-door)
  (ensure-layer "window")
  (ensure-layer "door")

  (if (not (layer-exists "DS-门窗"))
    (progn
      (alert "找不到图层 [DS-门窗]！\n请确认图层名是否正确。")
      (princ)
    )
  )

  (princ "\n========== 拆分 DS-门窗 ==========")
  (princ "\n  请手动框选图层 [DS-门窗] 中的【窗户】→ 回车")

  ;; 选取窗户
  (setq ss-win (ssget '((8 . "DS-门窗"))))
  (if (not ss-win)
    (princ "\n  未选中任何窗户，跳过")
    (progn
      (setq n-win (sslength ss-win))
      (vl-catch-all-apply 'vl-cmdf (list "_.CHPROP" ss-win "" "_LA" "window" ""))
      (princ (strcat "\n  " (itoa n-win) " 个窗户 → [window] 层"))
    )
  )

  ;; 剩余 → door
  (setq ss-door (ssget "X" '((8 . "DS-门窗"))))
  (if ss-door
    (progn
      (setq n-door (sslength ss-door))
      (vl-catch-all-apply 'vl-cmdf (list "_.CHPROP" ss-door "" "_LA" "door" ""))
      (princ (strcat "\n  剩余 " (itoa n-door) " 个 → [door] 层"))
    )
  )

  (delete-empty-layer "DS-门窗")

  (princ "\n  SPLIT2 完成！")
  (princ)
)

;; ============================================================================
;; 辅助函数
;; ============================================================================
(defun ensure-layer (name / layers)
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vl-catch-all-apply 'vla-add (list layers name))
)

(defun layer-exists (name / layers result)
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq result
    (vl-catch-all-apply 'vla-item (list layers name)))
  (not (vl-catch-all-error-p result))
)

(defun delete-empty-layer (name / layers lay ss)
  (if (not (layer-exists name))
    nil
    (progn
      (setq ss (ssget "X" (list (cons 8 name))))
      (if (not ss)
        ;; 图层为空，删除
        (progn
          (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
          (setq lay (vla-item layers name))
          (vl-catch-all-apply 'vla-delete (list lay))
          (princ (strcat "\n  已删除空图层 [" name "]"))
        )
        (princ (strcat "\n  图层 [" name "] 仍有实体，未删除"))
      )
    )
  )
)

;; ============================================================================
;; 加载提示
;; ============================================================================
(princ "\n========================================")
(princ "\n split_layers.lsp v1.0 已加载")
(princ "\n  SPLIT1 = 拆分 BS-承重墙柱 (选柱→col, 其余→wall)")
(princ "\n  SPLIT2 = 拆分 DS-门窗     (选窗→window, 其余→door)")
(princ "\n  完成后再用 BLD 合并 PUB_WALL → wall")
(princ "\n========================================")
(princ)
