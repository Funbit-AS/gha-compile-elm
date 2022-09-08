echo "Runnig compile.sh"

#npm install --location=global elm@0.19.1 uglify-js

echo "received:" ${Elm_Path} "as input"

filepaths=${Elm_Path}

echo "Current filepaths: "$filepaths

echo "Trying to parse filepaths"

read -a paths <<< "$filepaths"

echo "Finished parsing filepaths"

echo "Paths:"
for path in "${paths[@]}";
do
    cd ${Workspace}/wagtail/${Project_Name}/${path}/elm
    echo pwd
    elm make src/FirstHomePage.elm --output elm.js
    #make release
done