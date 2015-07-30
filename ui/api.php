<?php
	header('Content-Type: application/json');

	$aAction = isset($_REQUEST['action']) ? $_REQUEST['action'] : '';
	$aRet	 = array();

	switch($aAction) {
		case 'save':
			// Save the current schedule
			$aData = isset($_REQUEST['data']) ? $_REQUEST['data'] : null;

			if($aData != null) {
				$aRet['success'] = true;
				// TODO: save data here

			} else {
				$aRet['failure'] = true;
				$aRet['message'] = 'Nothing to be saved.';
			}
			break;

		case 'magic':
			// Automagically create the schedule using Emilio's dark magic tool.
			$aRet['success'] = true;
			$aRet['data'] = array();
			break;

		case 'load':
			// Load any previously saved schedule.
			$aRet['success'] = true;
			$aRet['data'] = array();
			break;

		default:
			// Automagically create the schedule using Emilio's dark magic tool.
			$aRet['failure'] = true;
			$aRet['message'] = 'Unknown action method: ' . $aAction;
			break;
	}

	echo json_encode($aRet);
?>
