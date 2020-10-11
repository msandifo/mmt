PRO idlshade, $
		lims=lims ,$
			f=f,$
			res=res,$
			bor=bpr,$
			v=v,$
			ct=ct, $
			sea_level=sea_level,$
			tiff=tiff, $
			name=name

DEFSYSV, '!SRTM3', '/Volumes/data/data/global/topography/ferranti/symlinks/'
DEFSYSV, '!SRTM3_V2', '/Volumes/data/data/global/topography/ferranti/symlinks/'

	IF  ~KEYWORD_SET(lims) THEN  lims= [-36.7, 140.5, -33, 144]
	IF  ~KEYWORD_SET(f) THEN  f=.6
	IF  ~KEYWORD_SET(res) THEN  res=3
	IF  ~KEYWORD_SET(bor) THEN   bor=.045
	IF  ~KEYWORD_SET(v) THEN   v=4.5
 ;	IF  ~KEYWORD_SET(ct) THEN     ct= -1
	IF  ~KEYWORD_SET(sea_level) THEN    sea_level=0
	IF  ~KEYWORD_SET(tiff) THEN    tiff=0
	IF  ~KEYWORD_SET(name) THEN    name="idl"



d1 =1930
d2 = 2006
vert=v
scale = 0.0025/f

;cr = [1, 9]

 i = obj_new('srtm', lims[1], lims[0], lims[3]-lims[1], lims[2]-lims[0],    re=res, /zero, ver=2, ct=ct )
;i->sea, 1
i -> image, /hist, ct=ct;, bot=-20;, sea_level= sea_level
fob =i->view(vert=vert,    im=im, true=3, re=1 ,  name=name ,/tiff )


END
