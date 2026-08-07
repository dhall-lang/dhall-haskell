#file3 imports file2 imports file1 imports k8s
#file4 combines all in one and imports k8s

echo file3 resolve
echo "(./file3.dhall).mkPod" | time dhall resolve > /tmp/result_resolved_separate.dhall

echo file4 resolve
echo "(./file4.dhall).mkPod" | time dhall resolve > /tmp/result_resolved_combined.dhall

echo file3 normalize
echo "(./file3.dhall).mkPod" | time dhall > /tmp/result_normed_separate.dhall

echo file4 normalize
echo "(./file4.dhall).mkPod" | time dhall > /tmp/result_normed_combined.dhall

#==>
#  resolved_separate == resolved_combined
#  normed_separate == normed_combined

ls -l /tmp/result*.dhall
