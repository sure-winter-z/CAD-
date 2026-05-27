;; ============================================================================
;; organize_layers.lsp  —  自动整理图层 (AutoLISP 全自动版)
;; ============================================================================
;;  用法:
;;    (load "D:/桌面/organize_layers.lsp")  →  输入 ORG  →  回车
;;
;;  自动完成:
;;    PUB_WALL      → wall
;;    BS-承重墙柱   → col (柱子) + wall (墙体)
;;    DS-门窗       → window (窗) + door (门) + wall (其余)
;;    验证: 最终只有 wall / col / door / window 四个目标层有实体
;; ============================================================================

;; ============================================================
;; 可配置关键词列表
;; ============================================================
(setq *col_kw*
  '("COL" "COLUMN" "S-COL" "A-COL" "柱" "柱子" "结构柱"))

(setq *door_kw*
  '("DOOR" "A-DOOR" "M-DOOR" "D-DOOR" "门" "平开门" "推拉门" "卷帘门"))

(setq *win_kw*
  '("WINDOW" "A-WINDOW" "A-GLAZ" "M-WIND" "WINS" "窗" "窗户" "天窗" "幕墙"))

;; 柱子的最大面积 (mm²) — 超过此值视为墙体
(setq *max_col_area* 500000)

;; ============================================================
;; 辅助函数
;; ============================================================

(defun mk-layer (name)
  "确保图层存在"
  (vl-catch-all-apply 'vla-add
    (list (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) name))
)

(defun layer-exists-p (name / layers r)
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq r (vl-catch-all-apply 'vla-item (list layers name)))
  (not (vl-catch-all-error-p r))
)

(defun match-kw (text kw-list / found upper)
  "检查 text 是否包含任一关键词 (不区分大小写)"
  (setq upper (strcase text))
  (setq found nil)
  (foreach kw kw-list
    (if (and (not found) (vl-string-search (strcase kw) upper))
      (setq found T)))
  found
)

(defun is-block-p (ent / dxf)
  "判断实体是否为块参照 (INSERT)"
  (setq dxf (entget ent))
  (= (cdr (assoc 0 dxf)) "INSERT")
)

(defun get-block-name (ent / dxf)
  "获取块参照的块名 (大写)"
  (setq dxf (entget ent))
  (strcase (cdr (assoc 2 dxf)))
)

(defun is-closed-shape-p (ent / dxf typ flags)
  "判断是否为封闭图形 (圆/椭圆/封闭多段线)"
  (setq dxf (entget ent))
  (setq typ (cdr (assoc 0 dxf)))
  (cond
    ((= typ "CIRCLE")  T)
    ((= typ "ELLIPSE") T)
    ((member typ '("LWPOLYLINE" "POLYLINE"))
     (setq flags (cdr (assoc 70 dxf)))
     (= (logand flags 1) 1))
    (T nil)
  )
)

(defun get-obj-area (ent / obj area)
  "获取封闭图形的面积，非封闭返回 -1"
  (if (is-closed-shape-p ent)
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (vlax-property-available-p obj 'Area)
        (vlax-get obj 'Area)
        -1))
    -1)
)

(defun move-entity (ent new-layer)
  "将实体移到指定图层"
  (vl-catch-all-apply
    '(lambda (e l)
       (vla-put-Layer (vlax-ename->vla-object e) l))
    (list ent new-layer))
)

(defun count-entities-on-layer (layer / ss)
  "统计图层上的实体数量"
  (setq ss (ssget "X" (list (cons 8 layer))))
  (if ss (sslength ss) 0)
)

(defun delete-layer-if-empty (name / layers lay n)
  "如果图层为空且不是系统层，删除它"
  (if (and (layer-exists-p name)
           (not (member (strcase name) '("0" "DEFPOINTS"))))
    (progn
      (setq n (count-entities-on-layer name))
      (if (= n 0)
        (progn
          (setq layers (vla-get-Layers
            (vla-get-ActiveDocument (vlax-get-acad-object))))
          (setq lay (vla-item layers name))
          (vl-catch-all-apply 'vla-delete (list lay))
          (princ (strcat "\n  已删除空图层: [" name "]"))
        )
        (princ (strcat "\n  图层 [" name "] 仍有 " (itoa n) " 个实体，未删除"))
      ))))

;; ============================================================
;; 第1步: PUB_WALL → wall
;; ============================================================
(defun step1-pub-wall (/ ss n i ent)
  (if (not (layer-exists-p "PUB_WALL"))
    (progn (princ "\n  [PUB_WALL] 不存在，跳过") 0)
    (progn
      (mk-layer "wall")
      (setq ss (ssget "X" '((8 . "PUB_WALL"))))
      (if (not ss)
        (progn (princ "\n  [PUB_WALL] 无实体") 0)
        (progn
          (setq n (sslength ss))
          (setq i 0)
          (repeat n
            (move-entity (ssname ss i) "wall")
            (setq i (1+ i)))
          (princ (strcat "\n  [PUB_WALL] → [wall]: " (itoa n) " 个实体"))
          (delete-layer-if-empty "PUB_WALL")
          n
        )))))

;; ============================================================
;; 第2步: BS-承重墙柱 → col + wall
;; ============================================================
(defun step2-bs-layer (/ ss n i ent bname area n-col n-wall)
  (setq n-col 0  n-wall 0)

  (if (not (layer-exists-p "BS-承重墙柱"))
    (progn (princ "\n  [BS-承重墙柱] 不存在，跳过") (list 0 0))
    (progn
      (mk-layer "col")
      (mk-layer "wall")
      (setq ss (ssget "X" '((8 . "BS-承重墙柱"))))

      (if (not ss)
        (progn (princ "\n  [BS-承重墙柱] 无实体") (list 0 0))

        (progn
          (setq n (sslength ss))
          (setq i 0)
          (repeat n
            (setq ent (ssname ss i))

            (cond
              ;; 块参照 → 按块名判断
              ((is-block-p ent)
               (setq bname (get-block-name ent))
               (if (match-kw bname *col_kw*)
                 (progn
                   (move-entity ent "col")
                   (setq n-col (1+ n-col)))
                 (progn
                   (move-entity ent "wall")
                   (setq n-wall (1+ n-wall)))))

              ;; 封闭图形 (圆/椭圆/封闭多段线) → 柱子（除非面积过大）
              ((is-closed-shape-p ent)
               (setq area (get-obj-area ent))
               (if (and (> area 0) (< area *max_col_area*))
                 (progn
                   (move-entity ent "col")
                   (setq n-col (1+ n-col)))
                 (progn
                   (move-entity ent "wall")
                   (setq n-wall (1+ n-wall)))))

              ;; 其余 (开放线段/弧/文字等) → 墙体
              (T
               (move-entity ent "wall")
               (setq n-wall (1+ n-wall)))
            )
            (setq i (1+ i))
          )

          (princ (strcat "\n  [BS-承重墙柱] → [col]: " (itoa n-col)
                         " / [wall]: " (itoa n-wall)))
          (delete-layer-if-empty "BS-承重墙柱")
          (list n-col n-wall)
        )))))

;; ============================================================
;; 第3步: DS-门窗 → window + door + wall
;; ============================================================
(defun step3-ds-layer (/ ss n i ent bname n-win n-door n-wall)
  (setq n-win 0  n-door 0  n-wall 0)

  (if (not (layer-exists-p "DS-门窗"))
    (progn (princ "\n  [DS-门窗] 不存在，跳过") (list 0 0 0))
    (progn
      (mk-layer "window")
      (mk-layer "door")
      (mk-layer "wall")
      (setq ss (ssget "X" '((8 . "DS-门窗"))))

      (if (not ss)
        (progn (princ "\n  [DS-门窗] 无实体") (list 0 0 0))

        (progn
          (setq n (sslength ss))
          (setq i 0)
          (repeat n
            (setq ent (ssname ss i))

            (if (is-block-p ent)
              (progn
                (setq bname (get-block-name ent))
                (cond
                  ((match-kw bname *win_kw*)
                   (move-entity ent "window")
                   (setq n-win (1+ n-win)))
                  ((match-kw bname *door_kw*)
                   (move-entity ent "door")
                   (setq n-door (1+ n-door)))
                  (T
                   (move-entity ent "wall")
                   (setq n-wall (1+ n-wall)))))
              ;; 非块实体 → 墙体
              (progn
                (move-entity ent "wall")
                (setq n-wall (1+ n-wall))))

            (setq i (1+ i))
          )

          (princ (strcat "\n  [DS-门窗] → [window]: " (itoa n-win)
                         " / [door]: " (itoa n-door)
                         " / [wall]: " (itoa n-wall)))
          (delete-layer-if-empty "DS-门窗")
          (list n-win n-door n-wall)
        )))))

;; ============================================================
;; 验证函数
;; ============================================================
(defun validate-layers (/ layers target result msg n)
  (setq target '("wall" "col" "door" "window"))
  (setq forbidden '("PUB_WALL" "BS-承重墙柱" "DS-门窗"))
  (setq result T)
  (setq msg "")

  (foreach name target
    (if (not (layer-exists-p name))
      (progn
        (setq result nil)
        (setq msg (strcat msg "\n  缺少图层: [" name "]")))
      (progn
        (setq n (count-entities-on-layer name))
        (if (= n 0)
          (progn
            (setq result nil)
            (setq msg (strcat msg "\n  图层 [" name "] 无实体")))))))

  (foreach name forbidden
    (if (layer-exists-p name)
      (progn
        (setq result nil)
        (setq msg (strcat msg "\n  旧图层未删除: [" name "]")))))

  (if result
    (princ "\n========== 成功: 图层整理完毕 ==========")
    (princ (strcat "\n========== 失败 ==========" msg)))

  (princ "\n  当前图层列表:")
  (vlax-for lay (vla-get-Layers
                  (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq n (count-entities-on-layer (vla-get-Name lay)))
    (if (> n 0)
      (princ (strcat "\n    [" (vla-get-Name lay) "] : " (itoa n) " 个实体"))))
  result
)

;; ============================================================
;; 主命令 ORG
;; ============================================================
(defun c:ORG (/ *error*)
  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (alert (strcat "运行出错: " msg)))
    (princ))

  (princ "\n========================================")
  (princ "\n  自动整理图层: PUB_WALL + BS-承重墙柱 + DS-门窗")
  (princ "\n========================================")

  (princ "\n[1/3] PUB_WALL → wall")
  (step1-pub-wall)

  (princ "\n\n[2/3] BS-承重墙柱 → col + wall")
  (step2-bs-layer)

  (princ "\n\n[3/3] DS-门窗 → window + door + wall")
  (step3-ds-layer)

  (princ "\n\n--- 验证 ---")
  (validate-layers)
  (princ)
)

;; ============================================================
;; 加载提示
;; ============================================================
(princ "\n========================================")
(princ "\n organize_layers.lsp 已加载")
(princ "\n 输入 ORG 一键整理图层")
(princ "\n========================================")
(princ)
