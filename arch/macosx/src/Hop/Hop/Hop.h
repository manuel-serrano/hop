//
//  Hop.h
//  Hop
//
//  Created by Marcela on 8/4/13.
//
//

#import <Foundation/Foundation.h>

@interface Hop : NSObject<NSCopying,NSCoding>


// Path of hop application
@property NSString *path;
// HTTP port number
@property NSString *port;
// Enable zeroconf support
@property BOOL zeroconf;
// Enable Web Dav 
@property BOOL webDav;
// Verbosity level
@property int verbose;
// Debug level
@property int debug;
// Additional arguments
@property NSString *arguments;

// Gets the list of start settings
-(NSArray *) getStartArgs;

// Gets the minimal list for starting
-(NSArray *) getMinStart;

// Gets the list of stop settings
-(NSArray *) getStopArgs;

// Gets the list of start settings
-(NSString *) getCommand:(BOOL) status;

@end
