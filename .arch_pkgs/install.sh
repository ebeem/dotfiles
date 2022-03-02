full_path=$(realpath $0)
dir_path=$(dirname $full_path)

paru --skipreview --noremovemake --noupgrademenu --noredownload --norebuild --batchinstall --needed -Syu - < $dir_path/.packages
