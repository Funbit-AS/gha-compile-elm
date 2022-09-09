npm install --location=global elm@0.19.1 uglify-js

filepaths=${Elm_Path}

read -a paths <<< "$filepaths"

for path in "${paths[@]}";
do
    cd ${Workspace}/wagtail/${Project_Name}/${path}/elm
    make release
done