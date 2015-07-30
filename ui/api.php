<?php
	header('Content-Type: application/json');

	// Constants and stuff
	define('DATABASE_FILE', 			dirname(__FILE__) . '/database.sqlite');
	define('DATABASE_DUMMY_DATA_FILE', 	dirname(__FILE__) . '/../inputs/teste3_json.out');

	// All the rest :D
	$aAction = isset($_REQUEST['action']) ? $_REQUEST['action'] : '';
	$aRet	 = array();
	$aDb	 = null;

	// Check if database already exists, otherwise
	// creates a new one
	if(!file_exists(DATABASE_FILE)) {
		$aDb = new SQLite3(DATABASE_FILE);

		// Create required tables
		$aDb->exec('CREATE TABLE IF NOT EXISTS schedules (id PRIMARY KEY, data STRING, time INTEGER)');

		// Insert some dummy data
		// TODO: use better dummy data.
		$aDummyData = file_get_contents(DATABASE_DUMMY_DATA_FILE);
		$aDb->exec("INSERT INTO schedules (id, data) VALUES (1, '".$aDummyData."')");
	}

	// Get DB ready to use.
	$aDb = new SQLite3(DATABASE_FILE);

	switch($aAction) {
		// Save the current schedule
		case 'save':
			$aData = isset($_REQUEST['data']) ? $_REQUEST['data'] : null;

			if($aData != null) {
				$aRet['success'] = true;
				// TODO: save data here

			} else {
				$aRet['success'] = false;
				$aRet['message'] = 'Nothing to be saved.';
			}
			break;

		// Automagically create the schedule using Emilio's dark magic tool.
		case 'magic':
			// TODO: replace it with a system() call
			$aOutput = file_get_contents(DATABASE_DUMMY_DATA_FILE);

			if($aOutput != null) {
				$aRet['success'] = true;
				$aRet['data'] = json_decode($aOutput);

			} else {
				// Something wrong just happened with the scheduler.
				$aRet['success'] = false;
				$aRet['message'] = 'Something wrong with the scheduler';
			}
			break;

		// Load any previously saved schedule.
		case 'load':
			$aRet['success'] = true;
			$aRet['data'] = array();

			$aMode = isset($_REQUEST['mode']) ? $_REQUEST['mode'] : 'raw';

			$aResults = $aDb->query('SELECT id, data FROM schedules WHERE 1');

			while ($aRow = $aResults->fetchArray()) {
				$aRet['data'] = json_decode($aRow['data']);
			}
			break;

		default:
			// Automagically create the schedule using Emilio's dark magic tool.
			$aRet['success'] = false;
			$aRet['message'] = 'Unknown action method: ' . $aAction;
			break;
	}

	echo json_encode($aRet);
?>
