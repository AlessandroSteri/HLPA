
echo "============="
echo " **  GIT **  "
echo "============="
git add .
echo "git status:"
git status
git commit -m "$*"
echo "-----> commited: $*"
git push origin master
echo "pushed origin master"
echo "-----> GIT LOG:"
git log
#git diff
