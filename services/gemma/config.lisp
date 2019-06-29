;; Example configuration file for Gemma

(config :port 4242
        :data-dir "/tmp/gemma/")

(deftask bathroom/wipe-mirror 7)
(deftask bathroom/wipe-counter 7)

;; Bedroom tasks
(deftask bedroom/change-sheets 7)
(deftask bedroom/vacuum 10)

;; Kitchen tasks
(deftask kitchen/normal-trash 3)
(deftask kitchen/green-trash 5)
(deftask kitchen/blue-trash 5)
(deftask kitchen/wipe-counters 3)
(deftask kitchen/vacuum 5 "Kitchen has more crumbs and such!")

;; Entire place
(deftask clean-windows 60)
