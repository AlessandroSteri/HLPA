
echo "============="
echo " **  GIT **  "
echo "============="
git add .
echo "git status:"
git status
git commit -m "$*"
echo "-----> commited: $*"
git push #origin master
#git push --force #forza push se local more updated than remote
echo "pushed origin master"
echo "-----> GIT LOG:"
#git log
#git diff
