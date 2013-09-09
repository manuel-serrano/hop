//
//  HopAppDelegate.h
//  Hop
//
//  Created by Marcela Rivera on 10/12/12.
//  Copyright (c) 2012 INRIA. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <CoreServices/CoreServices.h>

@class Hop;

@interface HopAppDelegate : NSObject <NSApplicationDelegate>;

// Main Window
@property (assign) IBOutlet NSWindow *window;

// Toolbar
@property (weak) IBOutlet NSToolbarItem *startButton;
@property (weak) IBOutlet NSToolbarItem *stopButton;
@property (weak) IBOutlet NSToolbarItem *statusIcon;

// Warning Label
@property (weak) IBOutlet NSTextField *unsavedWarning;
@property (weak) IBOutlet NSTextField *unloadWarning;

// Tabs
@property (weak) IBOutlet NSTabView *mainTabView;

// Console tab
@property (weak) IBOutlet NSScrollView *scrollView;
@property (unsafe_unretained) IBOutlet NSTextView *textOutput;

// Hop settings
@property (strong) Hop *hop;

// Settings tab
@property (weak) IBOutlet NSTabViewItem *settingTab;
@property (weak) IBOutlet NSTextField *portNumber;
@property (weak) IBOutlet NSButton *zeroConfStatus;
@property (weak) IBOutlet NSButton *webDavStatus;
@property (weak) IBOutlet NSSlider *verboseLevel;
@property (weak) IBOutlet NSSliderCell *debugLevel;
@property (weak) IBOutlet NSTextField *pathText;
@property (weak) IBOutlet NSTextField *argumentsText;

@property (weak) IBOutlet NSButtonCell *saveButton;
@property (weak) IBOutlet NSButton *revertButton;
@property (weak) IBOutlet NSButton *defaultButton;

// Menu
@property (weak) IBOutlet NSMenuItem *restoreMainMenuItem;
@property (weak) IBOutlet NSMenuItem *stopMenu;
@property (weak) IBOutlet NSMenuItem *runMenu;

// For starting hop when start button is pressed
- (IBAction)runHop:(id)sender;
// For stopping hop when stop button is pressed
- (IBAction)stopHop:(id)sender;
// Restore Main Windows when Main Window in the Menu is pressed
- (IBAction)restoreMain:(id)sender;
// For clearing the console
- (IBAction)clearConsole:(id)sender;
// For saving hop settings when Save Settings button is pressed
- (IBAction)saveSettingsButton:(id)sender;
// For restoring hop settings when Defaults button is pressed
- (IBAction)restore:(id)sender;
// For reverting the changes when Revert Settings button is pressed
- (IBAction)restoreDefaults:(id)sender;
- (IBAction)chooseHopPath:(id)sender ;
- (IBAction)verboseChanged:(id)sender ;
- (IBAction)debugChanged:(id)sender;
- (IBAction)webdavChanged:(id)sender ;
- (IBAction)zeroconfChanged:(id)sender ;
- (IBAction)loadSettings:(id)sender;
- (IBAction)saveSettingsAs:(id)sender ;
- (IBAction)SetNewDefaultSettings:(id)sender;
@end



