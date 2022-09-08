echo "Runnig compile.sh"

npm install --location=global elm@0.19.1 uglify-js

filepaths=${INPUT.ELM_PATHS}

echo "Trying to parse filepaths"

read -a paths <<< "$filepaths"

echo "Finished parsing filepaths"

echo "Paths:"
for path in "${paths[@]}";
do
    cd ${INPUT.GITHUB_WORKSPACE}/wagtail/${INPUT.PROJECT_NAME}/${path}/elm
    elm make src/FirstHomePage.elm --output elm.js
    #make release
done