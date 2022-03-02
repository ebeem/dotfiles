full_path=$(realpath $0)
dir_path=$(dirname $full_path)

$dir_path/pre-install.sh
$dir_path/install.sh
$dir_path/post-install.sh
