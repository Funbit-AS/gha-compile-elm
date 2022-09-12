echo "Running compile script ./compile.sh"

npm install --location=global elm@${ELM_VERSION} uglify-js

echo ""
echo "Received the following input: "${ELM_PATHS}

filepaths=${ELM_PATHS}

read -a paths <<< "$filepaths"

echo "Finished parsing filepaths"
echo ""
echo "Paths:"

for path in "${paths[@]}";
do
    echo ${path}
    cd ${WORKSPACE}${path}
    make release
done