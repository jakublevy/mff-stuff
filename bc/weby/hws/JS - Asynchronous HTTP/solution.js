/**
 * Data model for loading the work hour categories and fileld hours.
 * The model implements internal cache, so the data does not have to be
 * loaded every time from the REST API.
 */
class DataModel {

	/**
	 * Initialize the data model with given URL pointing to REST API.
	 * @param {string} apiUrl Api URL prefix (without the query part).
	 */
	constructor(apiUrl) {
		// this.apiUrl = apiUrl.substr(0, apiUrl.length - 1)
		this.apiUrl = apiUrl
	}

	async getHour(id) {
		const params = '?action=hours&id=' + id
		return new Promise( (resolve, reject) => {
		fetch(this.apiUrl + params).then(function(response) {
			if(response.ok) {
				response.json().then(function(data) {
					if(data.ok)
						resolve(data['payload']['hours'])
					else {
						if('error' in data)
							resolve({error: data.error})
						else resolve({error: ''})
					}
				})
			}
			else {
				// return {error: response.statusText}
				resolve({error: response.statusText})
			}
		})
		 })

	}

	/**
	 * Retrieve the data and pass them to given callback function.
	 * If the data are available in cache, the callback is invoked immediately (synchronously).
	 * Otherwise the data are loaded from the REST API and cached internally.
	 * @param {Function} callback Function which is called back once the data become available.
	 *                     The callback receives the data (as array of objects, where each object
	 *                     holds `id`, `caption`, and `hours` properties).
	 *                     If the fetch failed, the callback is invoked with two arguments,
	 *                     first one (data) is null, the second one is error message
	 */
	async getData(callback) {
		var that = this
		let pAll = []
		var errCalled = false
		
		if(typeof this.cache === 'undefined' || this.cache.length === 0) { //no cache available 
			this.cache = []
			fetch(this.apiUrl).then(function(response) {
				if(response.ok) {
					response.json().then(function(data) {
						if(data.ok) {
							for(let i = 0; i < data['payload'].length; ++i) {
								let p = that.getHour(data['payload'][i].id).then(function(hour) {
								if(typeof hour === 'number') {
									data['payload'][i].hours = hour
									that.cache[that.cache.length] = data['payload'][i]
								}
								else {
									that.cache = []
									if('error' in hour)
										callback(null, hour['error'])
									else
										callback(null, '')

									errCalled = true
								}
								
							})
							pAll[pAll.length] = p
							}
							Promise.all(pAll).then(function() {
								if(!errCalled)
									callback(that.cache)
							})
						}
						else {
							that.cache = []
							if('error' in data)
								callback(null, data.error)
							else 
								callback(null, '')
						}
					})
				}
				else {
					callback(null, data.statusText)
				}
			})
		}
		else {
			callback(this.cache)
		}
	}


	/**
	 * Invalidate internal cache. Next invocation of getData() will be forced to load data from the server.
	 */
	invalidate() {
		this.cache = []
	}

	
	/**
	 * Modify hours for one record.
	 * @param {number} id ID of the record in question.
	 * @param {number} hours New value of the hours (m)
	 * @param {Function} callback Invoked when the operation is completed.
	 *                            On failutre, one argument with error message is passed to the callback.
	 */
	async setHours(id, hours, callback = null)
	{
		var that = this
		const params = '?action=hours&id=' + id + '&hours=' + hours
		fetch(this.apiUrl + params, { method: 'POST', headers: {
  																    'Accept': 'application/json',
      															    'Content-Type': 'application/json'
    															  }
													 }).then(function(response){
												        if(response.ok) {
															response.json().then(function(data) {
																if(data.ok) {
																	that.updateHours(id, hours)
																	if(callback !== null)
																	callback()
																}
																else if (callback !== null) {
																	if('error' in data)
																		callback(data.error)
																	else 
																		callback('')
																}
															})
														}
														else {
															callback(response.statusText)
														}
													 })
   }

   async updateHours(id, hours) {
	   this.cache.find((x) => x.id == id).hours = hours
   }

}

// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { DataModel };
