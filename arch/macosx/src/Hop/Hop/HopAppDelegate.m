//
//  HopAppDelegate.m
//  Hop
//
//  Created by Marcela Rivera on 10/11/12.
//  Copyright (c) 2012 INRIA. All rights reserved.
//

#import "HopAppDelegate.h"
#import "Hop.h"

@implementation HopAppDelegate

// Status variables
BOOL status = FALSE;
BOOL restart = FALSE;
BOOL loadedChanges = TRUE;

// Images for buttons 
NSImage *startedIcon;
NSImage *stoppedIcon;
NSImage *startButton;
NSImage *restartButton;
NSImage *startedIcon;
NSImage *stoppedIcon;

// Lalels for buttons
NSString *startedLabel;
NSString *stoppedLabel;
NSString *startLabel;
NSString *restartLabel;

NSString *fileExtension;

// Path of file where is stocked the default settings
NSString *path;
NSString *defaultPath;
NSString *defaultHoprcPath;

Hop *runHop, *defaultHop;

// For doing at start
- (void) awakeFromNib {
    fileExtension = @"hsf";
    path = [NSString stringWithFormat:@"%@%@", NSHomeDirectory(),@"/.config/hop"];
    //path = [NSString stringWithFormat:@"%@%@", NSHomeDirectory(),@"/Library/Preferences/hop"];
    defaultPath = [NSString stringWithFormat:@"%@%@.%@", path, @"/default",fileExtension];
    defaultHoprcPath = [NSString stringWithFormat:@"%@%@",path,@"/hoprc.hop"];
    
    if( ![[NSFileManager defaultManager] fileExistsAtPath:defaultHoprcPath]){
        [self firstLaunchTest];
    }
    [self registeringObserverForText];
}

// Initialisation
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification{
    
    Hop *hop = [[Hop alloc] init];
    runHop = [[Hop alloc] init];
    defaultHop = [[Hop alloc] init];
    
    // Load a default settings
    Hop *aux = [self loadDefaultSettings];
    if(aux != NULL){
        defaultHop = [aux copy];
        runHop = [aux copy];
        hop = [aux copy];
    }
    
    [self setHop:hop];
    
    // Images
    stoppedIcon= [NSImage imageNamed:@"ic_red_light.png"];
    startedIcon= [NSImage imageNamed:@"ic_green_light.png"];
    startButton= [NSImage imageNamed:@"NSRightFacingTriangleTemplate"];
    restartButton= [NSImage imageNamed:@"ic_menu_refresh.png"];
    
    // Labels
    startedLabel = @"Started";
    stoppedLabel = @"Stopped";
    startLabel = @"Start";
    restartLabel = @"Restart";
    
    // Sets Setting interface
    [self updateInterface:defaultHop];
    
    [_unloadWarning setStringValue:@"New changes unloaded. You must restart hop. "];
    [_unsavedWarning setStringValue:@"Unsaved changes will not be kept." ];
    
    [_mainTabView selectFirstTabViewItem:self];
    
    // Verifies if the hop in path is disponible
    if (![self verifyPath:[runHop path]]) {
        [[[_textOutput textStorage] mutableString] appendString:  @"ERROR: hop launch path not accessible\n"];
    }
}

// Load the settings for default
- (Hop*)loadDefaultSettings{
    Hop *hop ;
    @try{
        NSData *data = [NSKeyedUnarchiver unarchiveObjectWithFile:defaultPath];
        if( data != NULL){
            hop = [NSKeyedUnarchiver unarchiveObjectWithData:data];
        }
    }
    @catch (NSException *exception) {
        [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\nERROR at loading default settings: ", [exception reason]] ];
    }
    return hop;
}

// Observer for textfields
- (void) registeringObserverForText{
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(textDidChange:)
                                                 name: NSControlTextDidChangeNotification
                                               object: _pathText];
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(textDidChange:)
                                                 name: NSControlTextDidChangeNotification
                                               object: _portNumber];
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(textDidChange:)
                                                 name: NSControlTextDidChangeNotification
                                               object: _argumentsText];
}

- (void) firstLaunchTest {
    Hop *hhop = [[Hop alloc] init];
    @try {
        [NSTask launchedTaskWithLaunchPath:[hhop path] arguments:[hhop getMinStart]] ;
        [[NSAlert alertWithMessageText:@"You have started Hop without any configuration file and without any user account."
                         defaultButton:@"OK"
                       alternateButton:nil
                           otherButton:nil
             informativeTextWithFormat:@"You must fix that in order to be able to use Hop.\n Hop wizard will be open in your browser for creating a simple Hop configuration file. Then, relaunch the Hop. \n Alternatively, for those that feel more confident, you can generate the configuration file by hand. The Hop documentation teaches how to proceed."]
         runModal ];
        NSURL *url = [ NSURL URLWithString: [ NSString stringWithFormat: @"http://localhost:8080/hop/wizard"] ];
        [[NSWorkspace sharedWorkspace] openURL:url];
        [NSApp terminate:self];
    }
    @catch (NSException *exception) {
        NSLog(@"%@", [exception reason]);
    }
}

// Modifications for changing Started to Stopped 
-(void) changeStartedtoStopped{
    [_statusIcon setLabel:stoppedLabel]; 
    [_statusIcon setImage:stoppedIcon];
    status = FALSE;
    [_stopButton setEnabled:FALSE];
    [_startButton setImage:startButton];
    [_startButton setLabel:startLabel];
    [_runMenu setTitle:startLabel];
    [_stopMenu setEnabled:NO];
}

// Modifications for changing Stopped to Started 
-(void) changeStoppedtoStarted{
    [_statusIcon setLabel:startedLabel];
    [_statusIcon setImage:startedIcon];
    status = TRUE;
    [_stopButton setEnabled:TRUE];
    [_startButton setImage:restartButton];
    [_startButton setLabel:restartLabel];
    [_runMenu setTitle:restartLabel];
    [_stopMenu setEnabled:YES];
}

// show warning messages
// 0 no messages
// 1 unsaved messages
// 2 unload messages
- (void) showWarningMessages:(int)messages{
    switch(messages) {
        case 0:
            [_unloadWarning setHidden:TRUE];
            [_unsavedWarning setHidden:TRUE];
            break;
        case 1:
            [_unloadWarning setHidden:TRUE];
            [_unsavedWarning setHidden:FALSE];
            break;
        case 2:
            [_unloadWarning setHidden:FALSE];
            [_unsavedWarning setHidden:TRUE];
    }
}

- (void) setButtons {
    if(![self isEqualtoItf:self.hop]){
        [_saveButton setEnabled:TRUE];
        [self showWarningMessages:1];
         [_revertButton setEnabled:TRUE];
    }else{
         [_revertButton setEnabled:FALSE];
        [_saveButton setEnabled:FALSE];
        if(status){
            if(loadedChanges){
               [self showWarningMessages:2];
            }else{
                [self showWarningMessages:0];
            }
        }else{
            [self showWarningMessages:0];
        } 
    }
    if([self isEqualtoItf:defaultHop]){
        [_defaultButton setEnabled:FALSE];
    }else{
        [_defaultButton setEnabled:TRUE];
    }
}

// Save new Settings
- (BOOL)saveSettingsMethod{
    BOOL saved = FALSE;
    int portValue = [[_portNumber stringValue] intValue];
    NSString *newPath = [[_pathText stringValue]stringByTrimmingCharactersInSet: [ NSCharacterSet whitespaceAndNewlineCharacterSet]];
    [_pathText setStringValue:newPath];
    NSString *newArguments = [self valideString:[_argumentsText stringValue]];
    [_argumentsText setStringValue:newArguments];
    if( portValue>1023 && portValue < 65536){
        if([self verifyPath:newPath]){
            if([self verifyArguments:[_argumentsText stringValue]]){
                [self.hop setPort:[NSString stringWithFormat:@"%d",portValue]];
                [_portNumber setStringValue: [self.hop port]];
                [self.hop setZeroconf:[_zeroConfStatus state]];
                [self.hop setWebDav:[_webDavStatus state]];
                [self.hop setVerbose:[_verboseLevel intValue]];
                [self.hop setDebug:[_debugLevel intValue]];
                [self.hop setPath:newPath];
                [self.hop setArguments:newArguments];
                if(status){
                    [self showWarningMessages:2];
                }
            saved = TRUE;
            loadedChanges = TRUE;
            }else{
                // Messages for unsaving
                [[NSAlert alertWithMessageText:@"Invalid arguments" defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat:@""] runModal ];
            }
        }else{
            [[NSAlert alertWithMessageText:@"Invalid path chosen! The file is not a executable file." defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat: @""] runModal ];
        }
    }else{
        // Messages for unsaving
        [[NSAlert alertWithMessageText:@"Invalid port number" defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat:@""] runModal ];
    }
    return saved;
}

- (NSString *) valideString: (NSString *)string{
    string = [string stringByTrimmingCharactersInSet: [ NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSError *error = nil;
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"  +" options:NSRegularExpressionCaseInsensitive error:&error];
    return[regex stringByReplacingMatchesInString:string options:0 range:NSMakeRange(0, [string length]) withTemplate:@" "];
}

- (BOOL) isEqualtoItf:(Hop *)hhop{
    return  [[_portNumber stringValue] isEqualToString:[hhop port]] &&
    [_zeroConfStatus state] == [hhop zeroconf] &&
    [_webDavStatus state] == [hhop webDav] &&
    [_debugLevel doubleValue] == [hhop debug] &&
    [_verboseLevel doubleValue] == [hhop verbose] &&
    ([[_pathText stringValue] isEqualToString:[hhop path]]) &&
    ([[_argumentsText stringValue] isEqualToString:[hhop arguments]]);
}

- (void) updateInterface: (Hop *)hhop{
    [_portNumber setStringValue:[hhop port]];
    [_zeroConfStatus setState:[hhop zeroconf]];
    [_webDavStatus setState:[hhop webDav]];
    [_debugLevel setDoubleValue:[hhop debug]];
    [_verboseLevel setDoubleValue:[hhop verbose]];
    [_pathText setStringValue:[hhop path]];
    [_argumentsText setStringValue:[hhop arguments]];
}

// Close the application definitely after close the last windows (disabled)
// - (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication {
//    return YES;
//}

- (void)textDidChange:(NSNotification *)aNotification{
    if([aNotification object] == _pathText || [aNotification object] == _portNumber || [aNotification object] == _argumentsText){
            [self setButtons];
    }
}

// Stops hop when the application is about to be terminated
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender{
    if (status) {
        [self changeStartedtoStopped];
        [[NSTask launchedTaskWithLaunchPath:[runHop path] arguments:[runHop getStopArgs]] waitUntilExit];
    }
    return YES;
}

// Reopen the window after cliking on dock icon
-(BOOL) applicationShouldHandleReopen:(NSApplication*)theApp hasVisibleWindows:(BOOL)v {
    [[self window] makeKeyAndOrderFront:nil];
    return YES;
}

// Stop Hop
- (void)stopHopTask{
    [self changeStartedtoStopped];
    [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@ %@", @"\n", [runHop getCommand:status], @"\n"]];
    [[NSTask launchedTaskWithLaunchPath:[runHop path] arguments:[runHop getStopArgs]] waitUntilExit];
}

// Launch Hop
- (void)runHopTask{
    
    runHop = [self.hop copy];
    
    // Pipe initialisation
    NSPipe *outPipe = [NSPipe pipe];
    NSPipe *errPipe = [NSPipe pipe];
    
    // Task initialisation with path, arguments and output
    NSTask *task;
    task = [[NSTask alloc] init];
    [task setLaunchPath: [runHop path]];
    [task setArguments: [runHop getStartArgs]];
    [task setStandardOutput: outPipe];
    [task setStandardError: errPipe];
    
    // For accessing to pipes
    NSFileHandle *outFile = [outPipe fileHandleForReading];
    NSFileHandle *errFile = [errPipe fileHandleForReading];
    [outFile waitForDataInBackgroundAndNotify];
    [errFile waitForDataInBackgroundAndNotify];
    
    // Monitor for capturing the messages when the task is finished
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(terminated:)
                                                 name:NSTaskDidTerminateNotification
                                               object:task];
    
    // Monitor for capturing the messages of standard output
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(outData:)
                                                 name:NSFileHandleDataAvailableNotification
                                               object:outFile];
    // Monitor for capturing the messages of error output
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(outData:)
                                                 name:NSFileHandleDataAvailableNotification
                                               object:errFile];
    // Task with hop is launched
    @try {
        [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@ %@", @"\n", [runHop getCommand:!status], @"\n"] ];
        [task launch];
        [self changeStoppedtoStarted];
        
        // Monitors are launched in an asynchronous mode
        dispatch_async(dispatch_get_global_queue(0, 0), ^{
            while (1){
                @autoreleasepool{
                    if (![[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]]){
                        break;
                    }
                }
            } 
        });
    }
    @catch (NSException *exception) {
        [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\nERROR: ", [exception reason]] ];
        [[NSNotificationCenter defaultCenter] removeObserver:self];
        [self registeringObserverForText];

    }
}

// Method for sending the output and error messages to TextView
- (void) outData: (NSNotification *) notification{
    NSFileHandle *file = (NSFileHandle*) [notification object];
    NSData *data = [file availableData];
    NSString *string = [[NSString alloc] initWithData:data encoding:NSASCIIStringEncoding];
    
    [file waitForDataInBackgroundAndNotify];
    
    [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\n", string] ];
    
    // Autoscrolling
    [_scrollView.contentView scrollToPoint:NSMakePoint(0, ((NSView*)_scrollView.documentView).frame.size.height - _scrollView.contentSize.height)];
}

// Method for sending the messages to TextView when the task is finished 
- (void) terminated: (NSNotification *)notification{
    [[[_textOutput textStorage] mutableString] appendString:  @"\nTask terminated\n"];
      // Autoscrolling
    [_scrollView.contentView scrollToPoint:NSMakePoint(0, ((NSView*)_scrollView.documentView).frame.size.height - _scrollView.contentSize.height)];
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [self registeringObserverForText];

    [self changeStartedtoStopped];
    if(restart){
        restart = FALSE;
        [self runHopTask];
    }
}

// Verify the existence of executable file
- (BOOL)verifyPath:(NSString *)testPath{		
    @try{
        NSTask *task;
        task = [[NSTask alloc] init];
        [task setLaunchPath: testPath ];
        return [[NSFileManager defaultManager] isExecutableFileAtPath:[task launchPath]];
    }
    @catch (NSException *exception) {
        [[[_textOutput textStorage] mutableString] appendString: @"\nERROR: "];
        [[[_textOutput textStorage] mutableString] appendString:[exception reason]];
    }
}

// Verify the correct syntax of additional arguments
- (BOOL)verifyArguments:(NSString *)testPath{
    @try{
        if([testPath length] == 0){
            return TRUE;
        }
           return TRUE;
    }
    @catch (NSException *exception) {
        [[[_textOutput textStorage] mutableString] appendString: @"\nERROR: "];
        [[[_textOutput textStorage] mutableString] appendString:[exception reason]];
    }
}

// Verify if changes are saved before restart
- (BOOL) saveChangesBeforeRun{
    BOOL run = TRUE;
    // Are there changes?
    if(![self isEqualtoItf:self.hop]){
        NSAlert* alert = [NSAlert alertWithMessageText: @"Do you want save the changes you made in Settings before restart hop?"
                                         defaultButton: @"Save and run"
                                       alternateButton: @"Run without save"
                                           otherButton: @"Cancel"
                             informativeTextWithFormat: @"Your changes will not be kept at running if you do not save."];
        NSInteger response = [alert runModal];
        switch(response){
            case NSAlertDefaultReturn:
                run = [self saveSettingsMethod];
                [self showWarningMessages:0];
                loadedChanges = FALSE;
                [_mainTabView selectFirstTabViewItem:self];
                [self setButtons];
                break;
            case NSAlertAlternateReturn:
                [_mainTabView selectFirstTabViewItem:self];
                [self showWarningMessages:1];
                run = TRUE;
                break;
            case NSAlertOtherReturn:
                run = FALSE;
                break;
        }
    }else{
       // runHop = [self.hop copy];
        [self showWarningMessages:0];
        loadedChanges = FALSE;
        [_mainTabView selectFirstTabViewItem:self];
    }
    return run;
}

// For starting hop when start button is pressed	
- (IBAction)runHop:(id)sender{
    if([self saveChangesBeforeRun]){
        if(!status){
            [self runHopTask];
        }else{
            restart = TRUE;
            [self stopHopTask];
        }
    }
}

// For stopping hop when stop button is pressed
- (IBAction)stopHop:(id)sender{
    if(status){
        restart = FALSE;
        [self stopHopTask];
    }
}

// Restore Main Windows when Main Window in the Menu is pressed
- (IBAction)restoreMain:(id)sender{
    [[self window] makeKeyAndOrderFront:nil];
}

// For clearing the console 	
- (IBAction)clearConsole:(id)sender{
    [_textOutput setString: @""];
}

// For saving hop settings when Save Settings button is pressed	
- (IBAction)saveSettingsButton:(id)sender{
    [self saveSettingsMethod];
    [self setButtons];
}

// For restoring hop settings when Defaults button is pressed
- (IBAction)restore:(id)sender{   
    [self updateInterface:self.hop];
    [self setButtons];
}
// For reverting the changes when Revert Settings button is pressed
- (IBAction)restoreDefaults:(id)sender{   
    [self updateInterface:defaultHop];
    [self setButtons];
}

- (IBAction)chooseHopPath:(id)sender {
    NSString *newPath;
    NSOpenPanel* openDlg = [NSOpenPanel openPanel];
    // Enable options in the dialog.
    [openDlg setCanChooseFiles:YES];
    [openDlg setCanChooseDirectories:YES];
    // [openDlg setAllowedFileTypes:fileTypesArray];
    [openDlg setAllowsMultipleSelection:FALSE];
    [openDlg setTitle:@"Choose Hop"];
    [openDlg setTreatsFilePackagesAsDirectories:TRUE];
    [openDlg setPrompt: @"Choose"];
    // [openDlg setMessage: @"Choose File"];
    
    if ( [openDlg runModal] == NSOKButton ) {
        NSArray *files = [openDlg URLs];
        newPath = [[files objectAtIndex:0] path];
        [_pathText setStringValue:newPath];
        [self setButtons];
    }
}

- (IBAction)verboseChanged:(id)sender {
    [self setButtons];
}

- (IBAction)debugChanged:(id)sender {
    [self setButtons];
}

- (IBAction)webdavChanged:(id)sender {
    [self setButtons];
}

- (IBAction)zeroconfChanged:(id)sender {
    [self setButtons];
}

- (IBAction)pathChanged:(id)sender {
     [self setButtons];
}


// Load a hop settings from a file
- (IBAction)loadSettings:(id)sender {
    NSOpenPanel* openDlg = [NSOpenPanel openPanel];
    // Enable options in the dialog.
    [openDlg setCanChooseFiles:YES];
    NSArray *fileTypesArray = [NSArray arrayWithObjects:fileExtension,nil];
    [openDlg setAllowedFileTypes:fileTypesArray];
    [openDlg setAllowsMultipleSelection:FALSE];
    [openDlg setTitle:@"Load Hop Settings"];
    [openDlg setTreatsFilePackagesAsDirectories:TRUE];
    [openDlg setPrompt: @"Load"];
    // [openDlg setMessage: @"kk File"];
    
    if ( [openDlg runModal] == NSOKButton ) {
        NSArray *files = [openDlg URLs];
        Hop *hop = [[Hop alloc]init];
        NSString *filePath = [[files objectAtIndex:0] path];
        @try{
            NSData *data = [NSKeyedUnarchiver unarchiveObjectWithFile: filePath];
            if(data != NULL){
                hop = [NSKeyedUnarchiver unarchiveObjectWithData:data];
                self.hop = [hop copy];
                [self updateInterface:self.hop];
                [self setButtons];
                [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@ %@ %@ %@", @"\nThe next command: \"", [self.hop getCommand:TRUE], @"has been loaded from file:",filePath , @"\"\n"]];
            }else{
                [[NSAlert alertWithMessageText:@"The setting file can not be loaded!" defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat:@""] runModal ];
            }
        }
        @catch (NSException *exception) {
            [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\n@ERROR at loading: ", [exception reason]] ];
        }
    }
}

// Save a hop settings to a file
- (IBAction)saveSettingsAs:(id)sender {
    NSSavePanel* saveDlg = [NSSavePanel savePanel];
    // Enable options in the dialog.
    NSArray *fileTypesArray = [NSArray arrayWithObjects:fileExtension,nil];
    [saveDlg setAllowedFileTypes:fileTypesArray];
    [saveDlg setTitle:@"Save Hop Settings"];
    [saveDlg setTreatsFilePackagesAsDirectories:TRUE];
    [saveDlg setPrompt: @"Save"];
    // [saveDlg setMessage: @"kk File"];
    
    if ( [saveDlg runModal] == NSOKButton ) {
        NSString *filePath = [[saveDlg URL] path];
        @try{
            NSData *data = [NSKeyedArchiver archivedDataWithRootObject:self.hop];
            if(data != NULL){
                [NSKeyedArchiver archiveRootObject:data toFile:filePath];
                [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@ %@ %@ %@", @"\nThe next command: \"", [self.hop getCommand:TRUE], @"is saved to file:",filePath , @"\"\n"]];
            }else{
                [[NSAlert alertWithMessageText:@"The setting file can not be saved!" defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat:@""] runModal ];
            }
        }
        @catch (NSException *exception) {
            [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\nERROR at saving: ", [exception reason]] ];
        }
    }
}

// Save the new default settings for running hop
- (IBAction)SetNewDefaultSettings:(id)sender {
    //NSString* filePath = [[NSBundle mainBundle] resourcePath];
    //filePath = [filePath stringByReplacingOccurrencesOfString:@"/Contents/Resources" withString:@"/Contents/default.hop"];
    @try{
        NSData *data = [NSKeyedArchiver archivedDataWithRootObject:self.hop];
        if(data != NULL){
            [NSKeyedArchiver archiveRootObject:data toFile:defaultPath];
            [self saveSettingsMethod];
            defaultHop = [self.hop copy];
            [self updateInterface:self.hop];
            [self setButtons];
            [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@ %@", @"\nThe next command has been configured by default: \n\"", [self.hop getCommand:TRUE], @"\"\n"]];
        }else{
            [[NSAlert alertWithMessageText:@"The setting file can not be saved!" defaultButton:@"Accept" alternateButton:nil otherButton:nil informativeTextWithFormat:@""] runModal ];
        }
    }
    @catch (NSException *exception) {
        [[[_textOutput textStorage] mutableString] appendString:[NSString stringWithFormat:@"%@ %@", @"\nERROR at saving: ", [exception reason]] ];
    }
    
}


@end

//// Archiving
//NSMutableArray *array = self.self.settingsList;
//[NSKeyedArchiver archiveRootObject:array toFile:[savePath stringByAppendingPathComponent: name]];
//
////UnArchiving
//NSMutableArray *array = [NSKeyedUnarchiver unarchiveObjectWithFile:[savePath stringByAppendingPathComponent: name]];
//if ([array count] > 0) {
//    self.settingsList = array;
//}


















