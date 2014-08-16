from datetime import datetime
from neareststation import returnNearestStation, googleDisDurAPI, readStations, distDur
from directions import directions
from flask import Flask, render_template, jsonify, request
app = Flask(__name__)
'''
# Initializations
'''
stations = readStations();

'''
# Home Page
'''
@app.route('/')
def hello_world():
	return render_template('index.html')
	
@app.route('/ryanspage')
def ryan():
	return 'All hail Ryan!'

'''
# Help Page
'''
@app.route('/api/')
@app.route('/api/help')
def api_help():
	return render_template('api_help.html')

'''
Mock data
'''
tripDuration = {'minutes':20}
station = {'id':1}

'''
# Services
'''
@app.route('/api/bikeduration', methods = ['GET'])
@app.route('/api/bikeDuration', methods = ['GET'])
def bikeDuration():
	# Read in Arguments
	start = request.args.get('start')
	end = request.args.get('end')
	weather = request.args.get('weather')
	return jsonify(tripDuration)

@app.route('/api/walkduration', methods = ['GET'])
@app.route('/api/walkDuration', methods = ['GET'])
def walkDuration():
	# Read in Arguments
	start = request.args.get('start')
	end = request.args.get('end')
	return jsonify(tripDuration)

@app.route('/api/closeststationwithbike', methods = ['GET'])
@app.route('/api/closestStationWithBike', methods = ['GET'])
@app.route('/api/closestStationWithDock', methods = ['GET'])
@app.route('/api/closeststationwithdock', methods = ['GET'])
@app.route('/api/nearest', methods = ['GET'])
def closestStation():
	poi = request.args.get('poi')
	if poi is None:
		return render_template('error.html')
	station = returnNearestStation(poi)
	json = {'ID': station[0], 'Name': station[1], 'Latitude': station[3], 'Longitude':station[4]}
	return jsonify(json)


@app.route('/api/bikePath', methods = ['GET'])
@app.route('/api/bikepath', methods = ['GET'])
def bikePath():
	# Read in Arguments
	start = request.args.get('origin')
	end = request.args.get('dest')
	if start is None:
		start = '633 Bay St, Toronto ON'
	if end is None:
		end = '633 Bay St, Toronto ON'
	return directions(start, end, 'bicycling').text;

@app.route('/api/walkPath', methods = ['GET'])
@app.route('/api/walkpath', methods = ['GET'])
def walkPath():
	# Read in Arguments
	start = request.args.get('origin')
	end = request.args.get('dest')
	if start is None:
		start = '633 Bay St, Toronto ON'
	if end is None:
		end = '633 Bay St, Toronto ON'
	return directions(start, end, 'walking').text;

# Error Page
@app.route('/error', methods =['GET'])
def error():
	return render_template('error.html')

'''
# Example Code And Tests
'''
# Returns the params
@app.route('/argtest', methods =['GET'])
def argtest():
	start = request.args.get('start')
	end = request.args.get('end')
	if start is None:
		start = 'N/A'
	if end is None:
		end = 'N/A'	
	return 'start: ' + start + ' end: '+end;	

# Return the Query String
@app.route('/returnstring/')
def adhoc_test():
	return request.query_string

# Call the nearest station
@app.route('/closest')
def closestToHome():
	station = returnNearestStation("45 Ulster, Toronto, ON")
	return "ID: " + station[0] + " Station: " + station[1] 

# # Test Case Page
@app.route('/test')
def testcase():
	return render_template('testcase.html')

# Test Knockout
@app.route('/knockout')
def knockout():
	return render_template('knockouttest.html')


'''
Main Function for Running Locally
'''
if __name__ == '__main__':
	app.run(debug = True)
