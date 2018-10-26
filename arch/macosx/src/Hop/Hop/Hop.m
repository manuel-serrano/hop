//
//  Hop.m
//  Hop
//
//  Created by Marcela on 8/4/13.
//
//

#import "Hop.h"

@implementation Hop

- (id)init{
    self = [super init];
    if(self){
        
        NSString* filePath = [[NSBundle mainBundle] bundlePath];

        self.path =[NSString stringWithFormat:@"%@%@",filePath,@"/Contents/Resources/hop"];
        //       self.path = @"/Applications/hop.app/Contents/MacOS/Hop";
        self.port =  @"8080";
        self.zeroconf =  FALSE;
        self.webDav = FALSE;
        self.verbose = 2;
        self.debug = 2;
        self.arguments = [[NSString alloc] init];
    }
    return self;
}

// Gets the list of start settings
-(NSArray *) getStartArgs{
    NSMutableArray *args = [NSMutableArray arrayWithObjects:@"-p",self.port,nil];
    
    if(self.zeroconf){
        [args addObject:@"--zeroconf"];
    }else{
        [args addObject:@"--no-zeroconf"];
    }
    
    [args addObject:@"--eval"];
    if(self.webDav){
        [args addObject:@"(hop-enable-webdav-set! #t)"];
    }else{
        [args addObject:@"(hop-enable-webdav-set! #f)"];
    }
    [args addObject:[NSString stringWithFormat:@"-v%d",self.verbose]];
    [args addObject:[NSString stringWithFormat:@"-g%d",self.debug]];
    [args addObject: @"--accept-kill"];
    [args addObject: @"--no-color"];
    if([self.arguments length] != 0){
        [args addObjectsFromArray:[[self arguments] componentsSeparatedByString:@" "]];
    }
    return [NSArray arrayWithArray:args];
};

// Gets the minimal list for starting
-(NSArray *) getMinStart{
    NSMutableArray *args = [NSMutableArray arrayWithObjects:@"-p",self.port,nil];
    [args addObject: @"--accept-kill"];
    return [NSArray arrayWithArray:args];
};

// Gets the list of stop settings
-(NSArray *) getStopArgs{
    NSMutableArray *args = [NSMutableArray arrayWithObjects:@"-p",self.port,@"-k",nil];
    return [NSArray arrayWithArray:args];
};

// Gets the start command
-(NSString *) getCommand:(BOOL)status{
    NSString *args = [[NSString alloc] init];
    args = [args stringByAppendingString:[self path]];
    args = [args stringByAppendingString:@" "];
    if(status){
        args = [args stringByAppendingString:[[NSMutableArray arrayWithArray:self.getStartArgs] componentsJoinedByString: @" "]];
    }else{
        args = [args stringByAppendingString:[[NSMutableArray arrayWithArray:self.getStopArgs] componentsJoinedByString: @" "]];
    }
    return args;
};

-(id)copyWithZone:(NSZone *)zone{
    Hop *newHop = [[Hop allocWithZone: zone] init];
    [newHop setPath:[[NSString alloc]initWithString:self.path]];
    [newHop setPort:[[NSString alloc]initWithString:self.port]];
    [newHop setZeroconf:self.zeroconf];
    [newHop setWebDav:self.webDav];
    [newHop setVerbose:self.verbose];
    [newHop setDebug:self.debug];
    [newHop setArguments:self.arguments];
    return newHop;
}

- (void)encodeWithCoder:(NSCoder *)aCoder{
    [aCoder encodeObject:self.path forKey:@"path"];
	[aCoder encodeObject:self.port forKey:@"port"];
	[aCoder encodeBool:self.zeroconf forKey:@"zeroconf"];
    [aCoder encodeBool:self.webDav forKey:@"webDav"];
    [aCoder encodeInt32:self.verbose forKey:@"verbose"];
    [aCoder encodeInt32:self.debug forKey:@"debug"];
    [aCoder encodeObject:self.arguments forKey:@"arguments"];
}

-(id)initWithCoder:(NSCoder *)coder {
	if ((self = [super init])){
        self.path = [coder decodeObjectForKey:@"path"];
        self.port = [coder decodeObjectForKey:@"port"] ;
        self.zeroconf = [coder decodeBoolForKey:@"zeroconf"];
        self.webDav = [coder decodeBoolForKey:@"webDav"];
        self.verbose = [coder decodeInt32ForKey:@"verbose"];
        self.debug = [coder decodeInt32ForKey:@"debug"];
        self.arguments = [coder decodeObjectForKey:@"arguments"] ;
 	}
	return self;
}

@end

