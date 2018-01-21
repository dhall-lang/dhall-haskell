let GitHubProject : Type = { owner : Text, repo : Text } in
let gitHubProject = \( github : GitHubProject ) ->
     let gitHubRoot = "https://github.com/${github.owner}/${github.repo}"
	 in { bugReports = "${gitHubRoot}/issues"  }
in gitHubProject
