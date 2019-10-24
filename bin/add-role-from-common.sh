#! /bin/zsh

original_dir=$(pwd)

cd $1
# cp -r roles/common roles/$2
mkdir -p roles/$2/{tasks,handlers,templates,files,vars,defaults,meta,library,module_utils,lookup_plugins}
touch roles/$2/{tasks,handlers,templates,files,vars,defaults,meta}/main.yml
cd ${original_dir}
