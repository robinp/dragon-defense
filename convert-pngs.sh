for i in $(ls *png); do convert $i $(basename $i .png).bmp; done
