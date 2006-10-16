/*
 * hop-sound.js			-- Sound Support for HOP
 * 
 * Copyright © 2006 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
 * 
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
 * USA.
 * 
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date: 12-Oct-2006 10:05 (eg)
 * Last file update: 16-Oct-2006 12:17 (eg)
 */

var flashproxy;
var soundCounter = 0;
var soundPool = new Array();

function hop_sound_init() {
    var uid = new Date().getTime();
    flashProxy = new FlashProxy(uid, 'JavaScriptFlashGateway.swf');

    var tag = new FlashTag('HopSound.swf', 600, 325);
    tag.setFlashvars('lcId='+uid);
    tag.write(document);
}

function hop_make_sound(url) {
    var snd = new Object();
    
    snd.id            = soundCounter++;
    snd.url           = url;
    snd.volume	      = 100;
    snd.pan	      = 0;
    soundPool[snd.id] = snd;
    return snd;
}

function hop_sound_load(snd, streaming) {
    var str = (streaming == undefined) ?  true : streaming;
    // Problem when deserializing booleans ==> Use an int.
    flashProxy.call('new_sound', snd.id, snd.url, str? 1 : 0);
}

function hop_sound_start(snd, loop)
{
    if (loop == undefined) loop = 1;
    flashProxy.call('start', snd.id, loop);
}

function hop_sound_stop(snd)
{
    flashProxy.call('stop', snd.id);
}

function hop_sound_pause(snd)
{
    flashProxy.call('pause', snd.id);
}

function hop_sound_get_volume(snd)
{
    return snd.volume
}

function hop_sound_set_volume(snd, value)
{
    flashProxy.call('setVolume', snd.id, value);
    snd.volume = value;
}

function hop_sound_get_pan(snd)
{
    return snd.pan;
}

function hop_sound_set_pan(snd, value)
{
    flashProxy.call('setPan', snd.id, value);
    snd.pan = value;
}

/* ====================================================================== *\
 *  
 * Handlers 
 * 
\* ====================================================================== */

function hop_sound_onLoadHandler(id, success)
{
    var snd = soundPool[id];

    if (snd.onLoad != undefined) {
	snd.onLoad(success);
    }
}

function hop_sound_onCompleteHandler(id)
{
    var snd = soundPool[id];

    if (snd.onSoundComplete != undefined) {
	snd.onSoundComplete();
    }
}

// Initialize the sound system 
hop_sound_init();
