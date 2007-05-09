unit Strings;

interface

const
	SDocNotInProject =
		'The document cannot be published because it is not on the project ' +
		'source path.'#13 +
		'To include this document in the project, save it under the project ' +
		'source folder.';
	SNeedProjectFolders =
		'Before publishing documents, you must set up project folders ' +
		'using Project|Project Options... from the menu.';
	SCreatePublishFolderQuery =
		'The Publish Folder "%s" specified for this project does not exist. ' +
		'Create this folder?';

implementation

end.
