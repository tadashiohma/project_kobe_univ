

for item in ["bibun_y1_multi", "bibun_y2_multi", "bibun_y4_multi"]:
    tmp = open("%s_128.f90" % item).read()
    for n in [32, 48, 64, 96, 192, 256, 512]:
        k = int(5000 *128**3 /n**3)
        with open("%s_%d.f90" % (item, n), "w") as fh:
            fh.write(
                tmp.replace("=128", "=%d" % n).replace("=5000", "=%d" % k)
            )

