;; ============================================================================
;; change.lsp  —  使用 LAYMRG 合并图层 (v2.0)
;; ============================================================================
;;  用法: (load "D:/桌面/CAD-/change.lsp") → CHG → 回车
;;
;;  功能:
;;    1. 检查 wall / window 图层是否存在，不存在则创建
;;    2. PUB_WALL + BS-承重墙柱 → LAYMRG 合并到 0 图层
;;    3. DS-门窗 → LAYMRG 合并到 window 图层
;;    4. 将 0 图层中的对象（块定义除外）转移到 wall 图层
;; ============================================================================

(defun c:CHG (/ *error* doc layers ss n)

  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg)))
    (princ))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  (princ "\n========================================")
  (princ "\n  change.lsp v2.0 — LAYMRG 图层合并")
  (princ "\n========================================")

  ;; ============================================================
  ;; 第1步: 检查 wall / window 图层，不存在则创建
  ;; ============================================================
  (princ "\n\n[1/4] 检查 wall / window 图层...")

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
  (princ "\n\n[2/4] LAYMRG: PUB_WALL + BS-承重墙柱 → 0...")

  (foreach source '("PUB_WALL" "BS-承重墙柱")
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers source)))
      (princ (strcat "\n  图层 [" source "] 不存在，跳过"))

      (progn
        (if (ssget "X" (list (cons 8 source)))
          (progn
            (command "_.LAYMRG"
              (ssget "X" (list (cons 8 source)))
              ""
              (tblobjname "LAYER" "0")
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
  (princ "\n\n[3/4] LAYMRG: DS-门窗 → window...")

  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-item (list layers "DS-门窗")))
    (princ "\n  图层 [DS-门窗] 不存在，跳过")

    (progn
      (if (ssget "X" (list (cons 8 "DS-门窗")))
        (progn
          (command "_.LAYMRG"
            (ssget "X" (list (cons 8 "DS-门窗")))
            ""
            (tblobjname "LAYER" "window")
          )
          (princ "\n  [DS-门窗] → [window] (LAYMRG)")
        )
        (princ "\n  图层 [DS-门窗] 无实体，跳过")
      )
    )
  )

  ;; ============================================================
  ;; 第4步: 将 0 图层的对象转移到 wall 图层
  ;;        等效于: 选中对象 → Ctrl+1 → 图层下拉 → 选 wall
  ;; ============================================================
  (princ "\n\n[4/4] 转移 0 图层对象 → wall...")

  ;; 只选模型空间和图纸空间的实体 (ssget "X" 不包含块定义内部)
  (setq ss (ssget "X" '((8 . "0"))))
  (if (not ss)
    (princ "\n  0 图层无实体，跳过")
    (progn
      (setq n (sslength ss))
      ;; CHPROP: 选中对象 → 改图层 → wall
      (command "_.CHPROP" ss "" "_LA" "wall" "")
      (princ (strcat "\n  0 → wall: " (itoa n) " 个对象已转移"))
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

  ;; 检查 wall 图层有实体
  (setq ss (ssget "X" (list (cons 8 "wall"))))
  (if ss
    (princ (strcat "\n  ✓ [wall] 图层包含 " (itoa (sslength ss)) " 个实体"))
    (princ "\n  ✗ [wall] 图层无实体！")
  )

  ;; 显示有实体的图层
  (princ "\n\n  当前图层 (含实体):")
  (vlax-for lay layers
    (setq lname (vla-get-Name lay))
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if (and ss (> (sslength ss) 0))
      (princ (strcat "\n    [" lname "] : " (itoa (sslength ss)) " 个"))
    )
  )

  (princ "\n\n========== 完成 ==========")
  (princ)
)

;; ============================================================
;; 加载提示
;; ============================================================
(princ "\n========================================")
(princ "\n change.lsp v2.0 已加载")
(princ "\n 输入 CHG 一键合并图层")
(princ "\n   [1] 确保 wall / window 图层存在")
(princ "\n   [2] PUB_WALL + BS-承重墙柱 → 0")
(princ "\n   [3] DS-门窗 → window")
(princ "\n   [4] 0 图层对象 → wall")
(princ "\n========================================")
(princ)
