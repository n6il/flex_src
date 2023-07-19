all: DISKS

DISKS: DBASIC14.SDF

DBASIC14.SDF: DBASIC14.DMK
	dmk2sdf DBASIC14.DMK -o DBASIC14.SDF

DBASIC14.DMK: DBASIC14.DSK
	~/src/flex_50_coco/tools/dmk.py --template ~/src/flex_50_coco/templates/BLANK_FLEX_SSDD35T.DMK --import DBASIC14.DSK --flex --padding 18 --output DBASIC14.DMK

DBASIC14.DSK: DBASIC.SYS
	./vfs.sh

DBASIC.SYS:
	python3 decbrom2flexbin.py disk10.rom DISK10M.SYS 0xc000
	cat DISK10M.SYS FL_504F/DBASIC.SYS >DBASIC.SYS

clean:
	rm -f DBASIC* DISK10M.SYS
