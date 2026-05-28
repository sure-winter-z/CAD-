;; ============================================================================
;; organize_layers.lsp  —  自动整理图层 (AutoLISP 全自动版 v2.0)
;; ============================================================================
;;  用法:
;;    (load "D:/桌面/organize_layers.lsp")  →  输入 ORG  →  回车
;;
;;  自动完成:
;;    [预处理] 炸开所有块/参照 → 绑定外部参照 → 解组 → 修复块属性
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

;; 预处理最大尝试次数
(setq *max_xplode_attempts* 5)

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
;; 预处理: 统计图中 INSERT (块参照) 数量
;; ============================================================
(defun count-inserts (/ ss n i ent cnt)
  (setq cnt 0)
  (setq ss (ssget "X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (repeat n
        (setq ent (ssname ss i))
        (if (is-block-p ent)
          (setq cnt (1+ cnt)))
        (setq i (1+ i)))))
  cnt
)

;; ============================================================
;; 预处理: 绑定所有外部参照 (Xref)
;;   等效于: XREF → Bind → 全选 → OK
;;   失败原因: 外部参照是链接而非嵌入，需先绑定才能炸开
;; ============================================================
(defun pre-bind-xrefs (/ doc blks n-bound)
  (setq n-bound 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blks (vla-get-Blocks doc))
  (vlax-for blk blks
    (if (= (vla-get-IsXref blk) :vlax-true)
      (progn
        (princ (strcat "\n  发现外部参照: [" (vla-get-Name blk) "]"))
        (setq n-bound (1+ n-bound)))))
  (if (> n-bound 0)
    (progn
      (princ (strcat "\n  正在绑定 " (itoa n-bound) " 个外部参照..."))
      ;; -XREF B * = 绑定所有外部参照
      (vl-catch-all-apply 'vl-cmdf (list "_.-XREF" "_B" "*"))
      (princ (strcat "\n  已绑定 " (itoa n-bound) " 个外部参照")))
    (princ "\n  未发现外部参照"))
  n-bound
)

;; ============================================================
;; 预处理: 解组所有编组 (Group)
;;   失败原因: 编组内的对象被 EXPLODE 跳过
;;   修复方法: 遍历 GROUPS 字典，逐个删除编组对象
;; ============================================================
(defun pre-ungroup-all (/ doc groups cnt)
  (setq cnt 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq groups (vla-get-Groups doc))
  (if (> (vla-get-Count groups) 0)
    (progn
      (princ (strcat "\n  发现 " (itoa (vla-get-Count groups)) " 个编组"))
      (while (> (vla-get-Count groups) 0)
        (vl-catch-all-apply 'vla-delete (list (vla-item groups 0)))
        (setq cnt (1+ cnt)))
      (princ (strcat "\n  已解组 " (itoa cnt) " 个编组")))
    (princ "\n  未发现编组"))
  cnt
)

;; ============================================================
;; 预处理: 修复块定义 — 勾选 "允许分解"
;;   失败原因: 块定义中 "允许分解" 未勾选 → EXPLODE 跳过该块
;;   修复方法: 遍历所有块定义，设置 Explodable = :vlax-true
;;   注意: 外部参照和布局块不能设置 Explodable，需跳过
;; ============================================================
(defun pre-fix-block-explodable (/ doc blks cnt)
  (setq cnt 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blks (vla-get-Blocks doc))
  (vlax-for blk blks
    ;; 只处理普通块定义 (非外部参照、非布局、非匿名块)
    (if (and (= (vla-get-IsXref blk) :vlax-false)
             (= (vla-get-IsLayout blk) :vlax-false)
             (= (vla-get-Explodable blk) :vlax-false))
      (progn
        (vl-catch-all-apply 'vla-put-Explodable (list blk :vlax-true))
        (princ (strcat "\n  已修复块: [" (vla-get-Name blk) "] → 允许分解"))
        (setq cnt (1+ cnt)))))
  (if (= cnt 0)
    (princ "\n  所有块均已允许分解"))
  cnt
)

;; ============================================================
;; 预处理: 单轮 XPLODE — 炸开所有可炸开对象
;;   返回 nil 表示本轮无变化 (所有对象已是最底层)
;; ============================================================
(defun pre-xplode-one-pass (/ ss-before cnt-before ss-after cnt-after)
  (setq ss-before (ssget "X"))
  (setq cnt-before (if ss-before (sslength ss-before) 0))

  ;; XPLODE: Express Tools 命令。若不可用则回退到 EXPLODE
  (if (member "xplode" (mapcar 'strcase (arx)))
    (vl-catch-all-apply 'vl-cmdf (list "_.XPLODE" (ssget "X") "" ""))
    (vl-catch-all-apply 'vl-cmdf (list "_.EXPLODE" (ssget "X") ""))
  )

  (setq ss-after (ssget "X"))
  (setq cnt-after (if ss-after (sslength ss-after) 0))

  (if (> cnt-after cnt-before)
    (progn
      (princ (strcat "\n  炸开: " (itoa cnt-before) " → " (itoa cnt-after) " 实体 (+"
                     (itoa (- cnt-after cnt-before)) ")"))
      T)  ;; 有变化，继续循环
    (progn
      (princ (strcat "\n  炸开: " (itoa cnt-before) " 实体 (无变化)"))
      nil)  ;; 无变化，可能遇到障碍
  )
)

;; ============================================================
;; 预处理主函数: 循环炸开直到全部完成或达最大尝试次数
;; ============================================================
(defun preprocess-xplode (/ attempt changed blocked
                            n-xref n-group n-block n-inserts)
  (princ "\n========================================")
  (princ "\n  [预处理] XPLODE 全部对象...")
  (princ "\n========================================")

  (setq attempt 0)

  (while (< attempt *max_xplode_attempts*)
    (setq attempt (1+ attempt))
    (princ (strcat "\n\n--- 第 " (itoa attempt) " 轮 ---"))

    ;; 显示当前块参照数量
    (setq n-inserts (count-inserts))
    (princ (strcat "\n  当前块参照: " (itoa n-inserts) " 个"))

    ;; 如果没有块参照了，说明已经炸完
    (if (= n-inserts 0)
      (progn
        (princ "\n  所有块已炸开，无需继续")
        (setq attempt *max_xplode_attempts*))  ;; 跳出循环
      (progn
        ;; 尝试炸开
        (setq changed (pre-xplode-one-pass))

        (if changed
          (princ "\n  本轮有变化，继续下一轮")

          ;; 无变化 → 诊断障碍并修复
          (progn
            (princ "\n  炸开无变化，诊断障碍...")
            (setq blocked nil)

            ;; 诊断1: 外部参照 ?
            (setq n-xref (pre-bind-xrefs))
            (if (> n-xref 0) (setq blocked T))

            ;; 诊断2: 编组 ?
            (setq n-group (pre-ungroup-all))
            (if (> n-group 0) (setq blocked T))

            ;; 诊断3: 块 "允许分解" 未勾选 ?
            (setq n-block (pre-fix-block-explodable))
            (if (> n-block 0) (setq blocked T))

            ;; 如果三个检查都没发现问题，说明无法再炸
            (if (not blocked)
              (progn
                (princ (strcat "\n  剩余 " (itoa n-inserts)
                               " 个块参照无法炸开"
                               " (可能是属性块或第三方对象)"))
                (setq attempt *max_xplode_attempts*)))
          )
        )
      )
    )
  )

  ;; 最终统计
  (setq n-inserts (count-inserts))
  (princ (strcat "\n\n  预处理完成 — 剩余块参照: " (itoa n-inserts) " 个"))
  (princ "\n========================================\n")
)

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
  (princ "\n  自动整理图层: 预处理 + 图层整理 v2.0")
  (princ "\n========================================")

  ;; ---- 预处理: 炸开所有对象 ----
  (preprocess-xplode)

  ;; ---- 图层整理 ----
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
(princ "\n organize_layers.lsp v2.0 已加载")
(princ "\n 输入 ORG 一键完成: 预处理炸开 + 图层整理")
(princ "\n========================================")
(princ)
