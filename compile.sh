echo "Runnig compile.sh"

#npm install --location=global elm@0.19.1 uglify-js

echo "received:" ${INPUTS_ELM_PATHS} "as input"

filepaths=${INPUTS_ELM_PATHS}

echo "Current filepaths:" $filepaths

echo "Trying to parse filepaths"

read -a paths <<< "$filepaths"

echo "Finished parsing filepaths"

echo "Paths:"
for path in "${paths[@]}";
do
    cd ${INPUTS_GITHUB_WORKSPACE}/wagtail/${INPUTS_PROJECT_NAME}/${path}/elm
    echo pwd
    elm make src/FirstHomePage.elm --output elm.js
    #make release
done