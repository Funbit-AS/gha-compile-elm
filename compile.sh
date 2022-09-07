echo "Runnig compile.sh"

npm install --location=global elm@0.19.1 uglify-js

filepaths="first second third fourth fifth"

echo "Trying to parse filepaths"

read -a paths <<< "$filepaths"

echo "Finished parsing filepaths"

echo "Paths:"
for path in "${paths[@]}";
do
    printf "$path\n"
    cd wagtail/${INPUT.PROJECT_NAME}/${path}/elm
    make release
done