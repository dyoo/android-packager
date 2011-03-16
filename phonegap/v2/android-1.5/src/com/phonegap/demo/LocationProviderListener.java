package com.phonegap.demo;
/* License (MIT)
 * Copyright (c) 2008 Nitobi
 * website: http://phonegap.com
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * “Software”), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
import android.content.Context;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationManager;
import android.location.LocationListener;
import android.os.Bundle;
import android.util.Log;
import java.util.List;

public class LocationProviderListener implements LocationListener {
	
	private Location cLoc;
	private LocationManager mLocMan;
	private static final String LOG_TAG = "PhoneGap";
	private GeoListener owner;
    int interval;
	
	public LocationProviderListener(Context ctx, int interval, GeoListener m)
	{
		Log.d("LocationProviderListener", 
		      "Making new LocationProviderListener with interval " +
		      interval + " milliseconds");
		this.owner = m;
		this.mLocMan = (LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE);
		this.interval = interval;


		requestUpdates();
		Log.d("GpsListener", "Listener created");
	}
	

    // Request updates to the provider
    private void requestUpdates() {
	Criteria c = new Criteria();
	c.setCostAllowed(false);
	c.setAccuracy(Criteria.ACCURACY_FINE);
	String provider = mLocMan.getBestProvider(c, true);
	Log.d(LOG_TAG, "requestLocationUpdates with " + this.interval + " milliseconds");
	mLocMan.requestLocationUpdates(provider, this.interval, 0, this);
	if (cLoc == null) {
	    cLoc = mLocMan.getLastKnownLocation(provider);
	}
// 	List<String> providers = mLocMan.getProviders(c, true);
// 	for (String provider : providers) {
// 	}
    }



    public Location getLocation()
    {
	return cLoc;
    }
	
    public void onProviderDisabled(String provider) {
	// TODO Auto-generated method stub
	Log.d(LOG_TAG, "The provider " + provider + " is disabled");
    }

    public void onProviderEnabled(String provider) {
	// TODO Auto-generated method stub
	Log.d(LOG_TAG, "The provider "+ provider + " is enabled");
    }


    public void onStatusChanged(String provider, int status, Bundle extras) {
	// TODO Auto-generated method stub
	Log.d(LOG_TAG, "The status of the provider " + provider + " has changed");
	if(status == 0)
	    {
		Log.d(LOG_TAG, provider + " is OUT OF SERVICE");
	    }
	else if(status == 1)
	    {
		Log.d(LOG_TAG, provider + " is TEMPORARILY_UNAVAILABLE");
	    }
	else
	    {
		Log.d(LOG_TAG, provider + " is Available");
	    }
    }


    public void onLocationChanged(Location location) {
	Log.d(LOG_TAG, "The location has been updated!");
	cLoc = location;
	owner.success(location);
    }

    public boolean hasLocation() {
	return (cLoc != null);
    }

    public void stop()
    {
	mLocMan.removeUpdates(this);
    }


    public void pause() {
	mLocMan.removeUpdates(this);
    }

    public void resume() {
	this.requestUpdates();
    }
	
}
