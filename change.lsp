;; ============================================================================
;; change.lsp  —  使用 LAYMRG 合并图层
;; ============================================================================
;;  用法: (load "D:/桌面/CAD-/change.lsp") → CHG → 回车
;;
;;  功能:
;;    1. 检查 wall / window 图层是否存在，不存在则创建
;;    2. PUB_WALL + BS-承重墙柱 → LAYMRG 合并到 0 图层
;;    3. DS-门窗 → LAYMRG 合并到 window 图层
;; ============================================================================

(defun c:CHG (/ *error* doc layers)

  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg)))
    (princ))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  (princ "\n========================================")
  (princ "\n  change.lsp — LAYMRG 图层合并")
  (princ "\n========================================")

  ;; ============================================================
  ;; 第1步: 检查 wall / window 图层，不存在则创建
  ;; ============================================================
  (princ "\n\n[1/3] 检查 wall / window 图层...")

  (foreach name '("wall" "window")
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers name)))
      (progn
        (vla-add layers name)
        (princ (strcat "\n  已创建图层: [" name "]"))
      )
      (princ (strcat "\n  图层已存在: [" name "]"))
    )
  )

  ;; ============================================================
  ;; 第2步: PUB_WALL + BS-承重墙柱 → LAYMRG 合并到 0 图层
  ;; ============================================================
  (princ "\n\n[2/3] 合并 PUB_WALL + BS-承重墙柱 → 0...")

  (foreach source '("PUB_WALL" "BS-承重墙柱")
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers source)))
      (princ (strcat "\n  图层 [" source "] 不存在，跳过"))

      (progn
        ;; 检查图层上是否有实体
        (if (ssget "X" (list (cons 8 source)))
          (progn
            ;; 使用 LAYMRG 合并到 0 图层
            ;; LAYMRG: 先选源对象 → 回车 → 选目标对象
            (command "_.LAYMRG"
              (ssget "X" (list (cons 8 source)))  ;; 选源图层所有实体
              ""                                    ;; 回车确认
              (tblobjname "LAYER" "0")             ;; 选 0 图层
            )
            (princ (strcat "\n  [" source "] → [0] (LAYMRG)"))
          )
          (princ (strcat "\n  图层 [" source "] 无实体，跳过"))
        )
      )
    )
  )

  ;; ============================================================
  ;; 第3步: DS-门窗 → LAYMRG 合并到 window 图层
  ;; ============================================================
  (princ "\n\n[3/3] 合并 DS-门窗 → window...")

  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-item (list layers "DS-门窗")))
    (princ "\n  图层 [DS-门窗] 不存在，跳过")

    (progn
      (if (ssget "X" (list (cons 8 "DS-门窗")))
        (progn
          (command "_.LAYMRG"
            (ssget "X" (list (cons 8 "DS-门窗")))  ;; 选源图层所有实体
            ""                                        ;; 回车确认
            (tblobjname "LAYER" "window")            ;; 选 window 图层
          )
          (princ "\n  [DS-门窗] → [window] (LAYMRG)")
        )
        (princ "\n  图层 [DS-门窗] 无实体，跳过")
      )
    )
  )

  ;; ============================================================
  ;; 验证结果
  ;; ============================================================
  (princ "\n\n--- 验证 ---")

  (setq source-layers '("PUB_WALL" "BS-承重墙柱" "DS-门窗"))
  (foreach name source-layers
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers name)))
      (princ (strcat "\n  ✓ [" name "] 已删除"))
      (princ (strcat "\n  ✗ [" name "] 仍然存在"))
    )
  )

  ;; 显示当前所有有实体的图层
  (princ "\n\n  当前图层 (含实体):")
  (vlax-for lay layers
    (setq lname (vla-get-Name lay))
    (if (> (sslength (ssget "X" (list (cons 8 lname)))) 0)
      (princ (strcat "\n    [" lname "]"))
    )
  )

  (princ "\n\n========== 完成 ==========")
  (princ)
)

;; ============================================================
;; 加载提示
;; ============================================================
(princ "\n========================================")
(princ "\n change.lsp 已加载")
(princ "\n 输入 CHG 一键合并图层")
(princ "\n   PUB_WALL + BS-承重墙柱 → 0")
(princ "\n   DS-门窗 → window")
(princ "\n========================================")
(princ)
